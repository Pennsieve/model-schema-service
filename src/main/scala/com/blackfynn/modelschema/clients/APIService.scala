package com.blackfynn.modelschema.clients

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.blackfynn.auth.middleware.{ DatasetNodeId, Jwt, OrganizationNodeId }
import com.blackfynn.models.Feature
import com.blackfynn.modelschema._
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.util.HttpUtils.{ authHeader, traceIdHeader }
import com.blackfynn.modelschema.util.HttpUtils
import com.blackfynn.service.utilities.ContextLogger
import io.circe.Json
import io.circe.generic.auto._

import scala.concurrent.{ ExecutionContext, Future }

object APIService {

  /**
    * Create a new dataset.
    *
    * @param name
    * @param description
    * @param token
    * @param traceId
    * @param system
    * @param ec
    * @param mat
    * @param msContainer
    * @return
    */
  def createDataset(
    name: String,
    description: Option[String],
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[(Json, DatasetId, DatasetNodeId)] = {
    implicit val log: ContextLogger = msContainer.log

    for {
      json <- HttpUtils
        .postReturningJson(
          List(authHeader(token), traceIdHeader(traceId)),
          s"${msContainer.client.apiServiceUrl}/datasets",
          requests.apiservice.CreateDataset(name, description),
          msContainer.client.http.responder
        )
      intId <- json.hcursor
        .downField("content")
        .downField("intId")
        .as[Int]
        .map(DatasetId)
        .fold(Future.failed, Future.successful)
      nodeId <- json.hcursor
        .downField("content")
        .downField("id")
        .as[String]
        .map(DatasetNodeId)
        .fold(Future.failed, Future.successful)
    } yield (json, intId, nodeId)
  }

  /**
    * Given an organization ID, return the available feature flags
    *
    * @param organizationNodeId
    * @param token
    * @param traceId
    * @param system
    * @param ec
    * @param mat
    * @param msContainer
    * @return
    */
  def getFeatureFlags(
    organizationNodeId: OrganizationNodeId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[Seq[Feature]] = {
    implicit val log: ContextLogger = msContainer.log

    HttpUtils
      .getReturningJson(
        List(authHeader(token), traceIdHeader(traceId)),
        s"${msContainer.client.apiServiceUrl}/organizations/${organizationNodeId.value}",
        msContainer.client.http.responder
      )
      .flatMap {
        _.hcursor
          .downField("organization")
          .downField("features")
          .as[Seq[Feature]]
          .fold(Future.failed, Future.successful)
      }
  }
}
