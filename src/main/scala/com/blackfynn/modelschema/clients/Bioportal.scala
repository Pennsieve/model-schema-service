package com.blackfynn.modelschema.clients

import java.net.URLEncoder

import scala.concurrent.{ ExecutionContext, Future }
import scala.concurrent.duration._
import scala.util.{ Failure, Success }

import com.blackfynn.service.utilities.{
  HttpResponder,
  QueueHttpResponder,
  RetryableException
}

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.pattern.after
import akka.stream._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import io.circe.parser.decode
import io.circe.generic.auto._

case class BranchContainer(val bioportal: Bioportal)

/**
  * Classes to describe the structure of responses from bioportal
  */
case class BioportalResponseLinks(children: String)
case class BioportalResponseInner(
  prefLabel: String,
  links: BioportalResponseLinks
)
case class BioportalResponsePageLinks(nextPage: Option[String])
case class BioportalResponse(
  collection: List[BioportalResponseInner],
  nextPage: Option[Int],
  links: BioportalResponsePageLinks
)

/**
  * A single node in the bioportal hierarchy tree
  */
class BioportalHierarchyNode(
  val label: String,
  val children: List[BioportalHierarchyNode] = List.empty
) {
  /*
   * Display the tree from this node with a given offset
   */
  def display(offset: Int = 0): String = {
    (" " * offset) + s"$label\n" + children
      .sortBy(_.label)
      .map(_.display(offset + 2))
      .mkString("")
  }

  /*
   * Flatten this node and all of its children into a list of labels
   */
  def flatten(): List[String] = label :: children.flatMap(_.flatten)
}

/**
  * This class can access the bioportal API and build a class hierarchy
  */
class Bioportal(
  implicit
  system: ActorSystem,
  executionContext: ExecutionContext,
  materializer: ActorMaterializer,
  config: Config,
  http: HttpResponder
) extends StrictLogging {

  val apiKey = config.getString("bioportal.key")

  val BIOPORTAL_HOST = config.getString("bioportal.host")

  /*
   * Given a URI string, remove the bioportal host
   */
  def removeBioportalHostFromURI(uri: String): String = {
    require(uri contains BIOPORTAL_HOST)
    uri.substring(uri.indexOf(BIOPORTAL_HOST) + BIOPORTAL_HOST.length)
  }

  /*
   * Given an ontology, get the root path
   */
  def getBioportalRootsURI(ontology: String): String =
    s"$BIOPORTAL_HOST/ontologies/$ontology/classes/roots"

  /*
   * Given an ontology and a classId, get the path to this class's
   * children
   */
  def getBioportalClassURI(ontology: String, classId: String): String = {
    val encodedClassId = URLEncoder.encode(classId, "UTF-8")
    s"$BIOPORTAL_HOST/ontologies/$ontology/classes/$encodedClassId/children"
  }

  /*
   * Validate a bioportal class ID
   */
  def isClassIdValid(ontology: String, classId: String): Future[Boolean] = {
    val req = HttpRequest(
      HttpMethods.GET,
      removeBioportalHostFromURI(
        getBioportalClassURI(ontology, classId) + s"?apikey=$apiKey"
      )
    )
    for {
      resp <- http.responder(req)
    } yield {
      resp.entity.discardBytes()
      resp.status == StatusCodes.OK
    }
  }

  /*
   * Decode a BioportalResponse object from the given text.
   *
   * This will attempt to convert inner responses to full responses if
   * isInnerResponse is true
   */
  def decodeBioportalResponse(
    responseText: String,
    expectInnerResponse: Boolean = false
  ): Either[io.circe.Error, BioportalResponse] =
    if (expectInnerResponse)
      decode[List[BioportalResponseInner]](responseText).map(
        decoded =>
          BioportalResponse(
            collection = decoded,
            nextPage = None,
            links = BioportalResponsePageLinks(nextPage = None)
          )
      )
    else decode[BioportalResponse](responseText)

  /*
   * Make the given request to Bioportal, retry on retryable failures
   */
  def makeRequest(
    requestLink: String,
    expectInnerResponse: Boolean = false,
    maxRetries: Int = 3,
    retrySeconds: Int = 5,
    retryNumber: Int = 0,
    queryChar: Char = '?'
  ): Future[BioportalResponse] = {
    val request = HttpRequest(
      HttpMethods.GET,
      removeBioportalHostFromURI(requestLink + s"${queryChar}apikey=$apiKey")
    )
    (
      for {
        // add the request to the queue and consume the content of the response
        resp <- http.responder(request)
        consumedEntity <- resp.entity.toStrict(1.minute)
        content = consumedEntity.data.utf8String
      } yield
        decodeBioportalResponse(content, expectInnerResponse) match {
          case Left(error) =>
            resp.status.intValue match {
              case 429 =>
                throw RetryableException("Rate Limit Exceeded")
              case 404 => {
                // the API contains some broken links, just leave the
                // children list empty in these cases. We have no other way
                // of knowing what the children should be.
                logger.error(
                  s"Broken link - no children retrieved: $requestLink"
                )
                BioportalResponse(
                  List.empty,
                  None,
                  BioportalResponsePageLinks(None)
                )
              }
              case _ => {
                logger.error(
                  s"$error: Unexpected response from bioportal: ${content.take(1000)}..."
                )
                throw error
              }
            }
          case Right(result) => result
        }
    ).recoverWith {
      // retry on any retryable exceptions, after retrySeconds and
      // up to maxRetries
      case e: RetryableException if (retryNumber < maxRetries) => {
        logger.warn(
          s"${e.message}, retrying (${retryNumber + 1}/$maxRetries).."
        )
        after(retrySeconds.seconds, system.scheduler)({
          makeRequest(requestLink, expectInnerResponse, retryNumber + 1)
        })
      }
    }
  }

  /*
   * Build up full collection by advancing pages until there are no more next pages
   */
  def pageChildren(response: BioportalResponse): Future[BioportalResponse] =
    response.nextPage match {
      case None => Future(response)
      case Some(_) =>
        response.links.nextPage match {
          case None => {
            logger.error("Next page was non null, but there was no link.")
            Future(response)
          }
          case Some(pageLink) =>
            makeRequest(pageLink, queryChar = '&')
              .map(
                newResponse =>
                  newResponse.copy(
                    collection = newResponse.collection ++ response.collection
                  )
              )
              .transformWith {
                case Failure(e) => Future.failed(e)
                case Success(res) => pageChildren(res)
              }
        }
    }

  /*
   * Get all of the child nodes for a given depth, up to a maximum depth
   *
   * childLinks is a map of label -> link to children for this label
   */
  def getChildNodes(
    childLinks: Map[String, String],
    maxDepth: Int,
    innerResponse: Boolean = false,
    curDepth: Int = 0
  ): Future[List[BioportalHierarchyNode]] = Future.sequence(
    // map over all labels, retrieving the children for each one
    childLinks.map {
      case (label, link) =>
        // if we're at the max depth, return
        if (curDepth == maxDepth) Future(new BioportalHierarchyNode(label))
        else {

          // make a request for each label in childLinks, and additional
          // recursive requests for each generation until maxDepth is reached
          for {
            decodedChild <- makeRequest(link, innerResponse)
            fullyPaged <- pageChildren(decodedChild)
            nodes <- getChildNodes(
              fullyPaged.collection
                .map(entity => (entity.prefLabel, entity.links.children))
                .toMap,
              maxDepth,
              curDepth = curDepth + 1
            )
          } yield {
            logger.info(s"Depth ${curDepth}: Done with $label")
            new BioportalHierarchyNode(label, nodes)
          }
        }
    }.toList
  )

  /*
   * Get the hierarchy for the given ontology, up to the given
   * depth. If no depth is provided, get the full hierarchy.
   */
  def getOntologyHierarchy(
    ontology: String,
    depth: Int = Integer.MAX_VALUE
  ): Future[BioportalHierarchyNode] =
    getChildNodes(
      Map(ontology -> getBioportalRootsURI(ontology)),
      depth,
      innerResponse = true
    ).map(_.head)

  /*
   * Get a flattened hierarchy for a given ontology, starting from a given classId
   */
  def getFlattenedHierarchyFromClass(
    ontology: String,
    classId: String,
    depth: Int = 0
  ): Future[List[String]] = {
    getChildNodes(
      Map(ontology -> getBioportalClassURI(ontology, classId)),
      depth + 1
    ).map(_.head.flatten.drop(1).distinct)

  }
}

object Bioportal {

  // since bioportal ontologies can be very large, we limit the depth
  // to keep request times reasonable
  val MAX_DEPTH = 2

  def default(
    implicit
    system: ActorSystem,
    executionContext: ExecutionContext,
    materializer: ActorMaterializer,
    config: Config
  ): Bioportal = {
    implicit val http = QueueHttpResponder(
      host = config.getString("bioportal.host"),
      queueSize = 100000,
      rateLimit = config.getString("bioportal.rate_limit").toInt,
      https = false
    )
    new Bioportal
  }
}
