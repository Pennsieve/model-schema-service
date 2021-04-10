package com.blackfynn.modelschema.clients

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{
  ContentType,
  HttpEntity,
  HttpMethods,
  HttpRequest,
  HttpResponse,
  MediaTypes,
  StatusCodes
}
import akka.stream.ActorMaterializer
import com.blackfynn.auth.middleware.Jwt
import com.blackfynn.modelschema.managers.SchemaManager
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.{ responses, _ }
import com.blackfynn.modelschema.util.HttpUtils
import com.blackfynn.modelschema.util.HttpUtils.{ authHeader, traceIdHeader }
import com.blackfynn.service.utilities.ContextLogger
import io.circe.Json
import io.circe.generic.auto._
import io.circe.shapes._
import io.circe.parser.parse
import io.circe.syntax._

import scala.concurrent.{ ExecutionContext, Future }

object ModelService {

  // TODO: refactor with HttpUtils.postReturningJson method
  def createModel(
    model: TemplateSchemaRecord,
    datasetId: DatasetId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[ModelId] = {
    val payload: requests.modelservice.CreateOrUpdateModel =
      SchemaManager.toCreateModel(model)
    val uri =
      s"/datasets/${datasetId.value}/concepts"
    val request = HttpRequest(
      method = HttpMethods.POST,
      uri = uri,
      entity = HttpEntity(
        ContentType(MediaTypes.`application/json`),
        payload.asJson.noSpaces
      ),
      headers = List(authHeader(token), traceIdHeader(traceId))
    )

    msContainer.client.modelServiceHttp.responder(request).flatMap { response =>
      response.status match {
        case StatusCodes.Created =>
          HttpUtils.getBody(response).flatMap { body =>
            parse(body) match {
              case Left(failure) =>
                Future.failed(UnknownException(failure.toString))
              case Right(json) =>
                val id = json.hcursor.get[String]("id").right.get
                Future.successful(ModelId(id))
            }
          }
        // Everything else will be interpreted as an error originating from the model (concept) service:
        case status =>
          HttpUtils.getBody(response).flatMap { body =>
            msContainer.log.noContext.error(
              s"Received an unexpected status ($status) from the model service with body: $body"
            )
            HttpUtils
              .asError(response, (_, json: Json) => ModelServiceJsonError(json))
          }
      }
    }
  }

  // TODO: refactor with HttpUtils.postReturningJson method
  def updateModel(
    modelId: ModelId,
    datasetId: DatasetId,
    modelPayload: requests.modelservice.CreateOrUpdateModel,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[ModelId] = {
    val request = HttpRequest(
      method = HttpMethods.PUT,
      uri = s"/datasets/${datasetId.value}/concepts/${modelId.value}",
      entity = HttpEntity(
        ContentType(MediaTypes.`application/json`),
        modelPayload.asJson.noSpaces
      ),
      headers = List(authHeader(token), traceIdHeader(traceId))
    )

    msContainer.client.modelServiceHttp.responder(request).flatMap { response =>
      response.status match {
        case StatusCodes.OK =>
          HttpUtils.getBody(response).flatMap { body =>
            parse(body) match {
              case Left(failure) =>
                Future.failed(UnknownException(failure.toString))
              case Right(json) =>
                val id = json.hcursor.get[String]("id").right.get
                Future.successful(ModelId(id))
            }
          }
        // Everything else will be interpreted as an error originating from the model (concept) service:
        case e @ _ =>
          HttpUtils
            .asError(response, (_, json: Json) => ModelServiceJsonError(json))
      }
    }
  }

  def getModel(
    modelId: ModelId,
    datasetId: DatasetId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[responses.modelservice.ModelResponse] = {
    implicit val log: ContextLogger = msContainer.log

    HttpUtils.get[responses.modelservice.ModelResponse](
      List(authHeader(token), traceIdHeader(traceId)),
      s"/datasets/${datasetId.value}/concepts/${modelId.value}",
      msContainer.client.modelServiceHttp.responder, {
        case (StatusCodes.NotFound, _) => Future.failed(InvalidModelId(modelId))
        case (_, json) => Future.failed(ModelServiceJsonError(json))
      }
    )
  }

  def getModels(
    datasetId: DatasetId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[Seq[responses.modelservice.ModelResponse]] = {
    implicit val log: ContextLogger = msContainer.log

    HttpUtils.get[Seq[responses.modelservice.ModelResponse]](
      List(authHeader(token), traceIdHeader(traceId)),
      s"/datasets/${datasetId.value}/concepts",
      msContainer.client.modelServiceHttp.responder
    )
  }

  def createRelationship(
    relationship: TemplateSchemaRelationship,
    datasetId: DatasetId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[responses.modelservice.RelationshipResponse] = {
    implicit val log: ContextLogger = msContainer.log

    for {
      create <- SchemaManager.toCreateRelationship(relationship) match {
        case Left(err) => Future.failed(err)
        case Right(req) => Future.successful(req)
      }
      response <- HttpUtils.post[
        requests.modelservice.CreateRelationship,
        responses.modelservice.RelationshipResponse
      ](
        List(authHeader(token), traceIdHeader(traceId)),
        s"/datasets/${datasetId.value}/relationships",
        create,
        msContainer.client.modelServiceHttp.responder
      )
    } yield response
  }

  def getRelationships(
    datasetId: DatasetId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[Seq[responses.modelservice.RelationshipResponse]] = {
    implicit val log: ContextLogger = msContainer.log

    HttpUtils.get[Seq[responses.modelservice.RelationshipResponse]](
      List(authHeader(token), traceIdHeader(traceId)),
      s"/datasets/${datasetId.value}/relationships",
      msContainer.client.modelServiceHttp.responder
    )
  }

  def createLinkedProperty(
    linkedProperty: TemplateSchemaLinkedProperty,
    datasetId: DatasetId,
    fromModel: ModelId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[responses.modelservice.LinkedPropertyResponse] = {
    implicit val log: ContextLogger = msContainer.log

    for {
      create <- SchemaManager.toCreateLinkedProperty(linkedProperty) match {
        case Left(err) => Future.failed(err)
        case Right(req) => Future.successful(req)
      }
      response <- HttpUtils.post[
        requests.modelservice.CreateLinkedProperty,
        responses.modelservice.LinkedPropertyResponse
      ](
        List(authHeader(token), traceIdHeader(traceId)),
        s"/datasets/${datasetId.value}/concepts/${fromModel.value}/linked",
        create,
        msContainer.client.modelServiceHttp.responder
      )
    } yield response
  }

  def getLinkedProperties(
    modelId: ModelId,
    datasetId: DatasetId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[Seq[responses.modelservice.LinkedPropertyTargetResponse]] = {
    implicit val log: ContextLogger = msContainer.log

    HttpUtils.get[Seq[responses.modelservice.LinkedPropertyTargetResponse]](
      List(authHeader(token), traceIdHeader(traceId)),
      s"/datasets/${datasetId.value}/concepts/${modelId.value}/linked",
      msContainer.client.modelServiceHttp.responder
    )
  }

  /**
    * Ingests all schema-level models, relationships, and linked properties contained in a dataset.
    *
    * @param datasetId
    * @param token
    * @param system
    * @param ec
    * @param mat
    * @param msContainer
    * @return
    */
  def ingestDatasetSchema(
    datasetId: DatasetId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[responses.modelservice.DatasetSchemaResponse] = {
    implicit val clientContainer = msContainer.client
    for {
      models <- getModels(datasetId, token, traceId)
      modelsWithProperties <- Future.sequence(
        models.map(
          model =>
            getModelProperties(
              ModelId(model.id.toString),
              datasetId,
              token,
              traceId
            ).map(properties => model -> properties)
        )
      )
      relationships <- getRelationships(datasetId, token, traceId)
      linkedProperties <- Future.sequence(
        models
          .map(
            model =>
              getLinkedProperties(
                ModelId(model.id.toString),
                datasetId,
                token,
                traceId
              )
          )
      )
    } yield {
      val modelsAndProperties: Seq[responses.modelservice.ModelAndProperties] =
        modelsWithProperties.map {
          case (model, properties) =>
            responses.modelservice.ModelAndProperties(model, properties)
        }
      responses.modelservice.DatasetSchemaResponse(
        modelsAndProperties,
        relationships,
        linkedProperties.flatten.distinct
      )
    }
  }

  // TODO: refactor with HttpUtils.putReturningJson method
  /**
    * Creates model properties by way of interpreting the .json field of the given `TemplateSchemaRecord` instance
    * in the form expected by the JSON template uploader (the original use of the model schema service.
    *
    * Properties are JSON formatted as a map of the form:
    *
    * See SchemaManager#parseProperties() for details
    *
    * {
    *   "${Prop1}" -> Models/SchemaProperty.asJson,
    *   "${Prop2}" -> Models/SchemaProperty.asJson,
    *   ...,
    *   "${PropN}" -> Models/SchemaProperty.asJson
    * }
    *
    * @param template
    * @param datasetId
    * @param token
    * @param modelId
    * @param system
    * @param ec
    * @param mat
    * @param msContainer
    * @return
    */
  def createModelPropertiesFromJSONTemplate(
    template: TemplateSchemaRecord,
    datasetId: DatasetId,
    token: Jwt.Token,
    modelId: ModelId,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[Seq[responses.modelservice.SchemaPropertyResponse]] = {

    SchemaManager.toCreateModelPropertyRequestsForJSONTemplate(template) match {
      case Right(
          payload: Seq[requests.modelservice.CreateOrUpdateModelProperty]
          ) if payload.isEmpty =>
        Future.successful(Seq.empty)
      case Right(
          payload: Seq[requests.modelservice.CreateOrUpdateModelProperty]
          ) if !payload.isEmpty => {
        val request = HttpRequest(
          method = HttpMethods.PUT,
          uri =
            s"/datasets/${datasetId.value}/concepts/${modelId.value}/properties",
          entity = HttpEntity(
            ContentType(MediaTypes.`application/json`),
            payload.asJson.noSpaces
          ),
          headers = List(authHeader(token), traceIdHeader(traceId))
        )

        msContainer.client.modelServiceHttp.responder(request).flatMap {
          res: HttpResponse =>
            res.status match {
              case StatusCodes.OK =>
                HttpUtils
                  .unmarshallAs[Seq[
                    responses.modelservice.SchemaPropertyResponse
                  ]](res)
              case StatusCodes.BadRequest => {

                HttpUtils
                  .asError(res, (_, json: Json) => InvalidModelProperties(json))
              }
              // Everything else will be interpreted as an error originating from the model (concept) service:
              case _ =>
                HttpUtils
                  .asError(res, (_, json: Json) => ModelServiceJsonError(json))
            }
        }
      }
      case Left(err) => Future.failed(err)
    }
  }

  /**
    * Creates model properties by way of interpreting the .json field of the given `TemplateSchemaRecord` instance
    * as `Seq[responses.modelservice.SchemaPropertyResponse]`.
    *
    * @param template
    * @param datasetId
    * @param token
    * @param modelId
    * @param system
    * @param ec
    * @param mat
    * @param msContainer
    * @return
    */
  def createModelPropertiesFromAPIResponse(
    template: TemplateSchemaRecord,
    datasetId: DatasetId,
    token: Jwt.Token,
    modelId: ModelId,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[Seq[responses.modelservice.SchemaPropertyResponse]] = {
    implicit val log: ContextLogger = msContainer.log

    for {
      payload <- SchemaManager.toCreateModelPropertyRequestsForModelService(
        template
      ) match {
        case Left(err) => Future.failed(err)
        case Right(payload) => Future.successful(payload)
      }
      response <- if (payload.isEmpty) Future.successful(Seq.empty)
      else
        HttpUtils
          .put[Seq[requests.modelservice.CreateOrUpdateModelProperty], Seq[
            responses.modelservice.SchemaPropertyResponse
          ]](
            List(authHeader(token), traceIdHeader(traceId)),
            s"/datasets/${datasetId.value}/concepts/${modelId.value}/properties",
            payload,
            msContainer.client.modelServiceHttp.responder
          )
    } yield response
  }

  // TODO: refactor with HttpUtils.get method
  def getModelProperties(
    modelId: ModelId,
    datasetId: DatasetId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[Seq[responses.modelservice.SchemaPropertyResponse]] = {
    val request =
      HttpRequest(
        method = HttpMethods.GET,
        uri =
          s"/datasets/${datasetId.value}/concepts/${modelId.value}/properties",
        headers = List(authHeader(token), traceIdHeader(traceId))
      )

    msContainer.client.modelServiceHttp.responder(request).flatMap {
      res: HttpResponse =>
        res.status match {
          case StatusCodes.OK =>
            HttpUtils
              .unmarshallAs[Seq[responses.modelservice.SchemaPropertyResponse]](
                res
              )
          // Everything else will be interpreted as an error originating from the model (concept) service:
          case _ =>
            HttpUtils
              .asError(res, (_, json: Json) => ModelServiceJsonError(json))
        }
    }
  }
}
