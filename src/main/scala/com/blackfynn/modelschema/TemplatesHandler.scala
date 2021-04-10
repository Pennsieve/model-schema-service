package com.blackfynn.modelschema

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpHeader
import akka.http.scaladsl.model.headers.{ Authorization, OAuth2BearerToken }
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.blackfynn.auth.middleware.AkkaDirective._
import com.blackfynn.auth.middleware.Jwt.Claim
import com.blackfynn.auth.middleware.{
  DatasetPermission,
  Jwt,
  Validator,
  DatasetId => JwtDatasetId,
  OrganizationId => JwtOrganizationId
}
import com.blackfynn.http.server.definitions
import com.blackfynn.http.server.templates.{
  TemplatesResource,
  TemplatesHandler => GuardrailHandler
}
import com.blackfynn.modelschema.clients.ModelService
import com.blackfynn.modelschema.db.TemplateSchemaMapper
import com.blackfynn.modelschema.managers.SchemaManager
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.model.implicits._
import io.circe.syntax._

import scala.concurrent.{ ExecutionContext, Future }

/**
  * Operations on templates loaded via a JSON file.
  */
class TemplatesHandler(
  claim: Claim
)(implicit
  system: ActorSystem,
  executionContext: ExecutionContext,
  mat: ActorMaterializer,
  msContainer: Container
) extends GuardrailHandler {

  val token: Jwt.Token = Jwt.generateToken(claim)(msContainer.jwt)

  val authHeader: HttpHeader = Authorization(OAuth2BearerToken(token.value))

  /*
   * Ensure that this claim has access to the given organization
   */
  def hasOrganizationAccess(organizationId: Int): Future[Unit] =
    if (Validator.hasOrganizationAccess(
        claim,
        JwtOrganizationId(organizationId)
      ))
      Future.unit
    else
      Future.failed(ModelSchemaForbiddenOrganizationException(organizationId))

  /*
   * Ensure that this claim has access to the given dataset
   */
  def hasDatasetAccess(datasetId: Int): Future[Unit] =
    if (Validator.hasDatasetAccess(
        claim,
        JwtDatasetId(datasetId),
        DatasetPermission.ManageModelTemplates
      ))
      Future.unit
    else
      Future.failed(ModelSchemaForbiddenDatasetException(datasetId))

  def withAuthorization[T](
    organizationId: Int
  )(
    f: Unit => Future[T]
  ): Future[T] =
    hasOrganizationAccess(organizationId).flatMap(f)

  def withAuthorization[T](
    organizationId: Int,
    datasetId: Int
  )(
    f: Unit => Future[T]
  ): Future[T] =
    for {
      _ <- hasOrganizationAccess(organizationId)
      _ <- hasDatasetAccess(datasetId)
      result <- f(())
    } yield result

  /*
   * Get the schemas for an organization
   */
  override def getSchemasForOrganization(
    respond: TemplatesResource.getSchemasForOrganizationResponse.type
  )(
    organizationId: Int
  ): Future[TemplatesResource.getSchemasForOrganizationResponse] =
    withAuthorization(organizationId) { _ =>
      msContainer.db
        .run(
          TemplateSchemaMapper
            .getTemplateGallerySchemasForOrganization(
              OrganizationId(organizationId)
            )
            .map(
              (seq: Seq[TemplateSchemaRecord]) =>
                respond.OK(
                  seq
                    .map(_.toSchema(msContainer.prefixUrl).toDTO)
                    .toIndexedSeq
                )
            )
        )
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }

  /*
   * Create a new template for an organization
   */
  override def createTemplate(
    respond: TemplatesResource.createTemplateResponse.type
  )(
    organizationId: Int,
    template: definitions.CreateSchemaRequest
  ): Future[TemplatesResource.createTemplateResponse] =
    withAuthorization(organizationId) { _ =>
      SchemaManager
        .create(OrganizationId(organizationId), template.toDomainObject)
        .map((rec: TemplateSchemaRecord) => respond.Created(rec.toDTO))
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: SchemaValidationException =>
          Future.successful(
            respond.BadRequest(
              definitions.ValidationBadRequest(
                e.message,
                problems = Some(e.payload.toIndexedSeq)
              )
            )
          )
        case e: InvalidSchemaUrl =>
          Future.successful(
            respond.BadRequest(definitions.ValidationBadRequest(e.message))
          )
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
        case e: InvalidModelProperties =>
          Future.successful(
            respond.BadRequest(definitions.ValidationBadRequest(e.message))
          )
        case e @ InvalidBranchPropertiesFound(properties) =>
          Future.successful(
            respond.BadRequest(
              definitions.ValidationBadRequest(e.message, Some(properties.map {
                case (name, error) => s"$name: $error"
              }.toIndexedSeq))
            )
          )
        case e: InvalidPropertyNamesFound =>
          Future.successful(
            respond.BadRequest(
              definitions.ValidationBadRequest(
                e.message,
                problems = Some(e.names.toIndexedSeq)
              )
            )
          )
        case e: InvalidDefaultValuesFound =>
          Future.successful(
            respond.BadRequest(
              definitions.ValidationBadRequest(
                e.message,
                problems = Some(e.names.toIndexedSeq)
              )
            )
          )
      }
    }

  /*
   * Get a schema by org ID and template ID
   */
  override def getSchema(
    respond: TemplatesResource.getSchemaResponse.type
  )(
    organizationId: Int,
    templateId: String
  ): Future[TemplatesResource.getSchemaResponse] =
    withAuthorization(organizationId) { _ =>
      msContainer.db
        .run(
          TemplateSchemaMapper
            .getSchema(OrganizationId(organizationId), ModelId(templateId))
        )
        .map(
          (tsr: TemplateSchemaRecord) =>
            respond.OK(tsr.toSchema(msContainer.prefixUrl).toDTO)
        )
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: InvalidSchemaId =>
          Future.successful(respond.NotFound(e.message))
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }

  /*
   * Delete a schema
   */
  override def deleteSchema(
    respond: TemplatesResource.deleteSchemaResponse.type
  )(
    organizationId: Int,
    templateId: String
  ): Future[TemplatesResource.deleteSchemaResponse] = {
    withAuthorization(organizationId) { _ =>
      msContainer.db
        .run(
          TemplateSchemaMapper
            .deleteSchema(OrganizationId(organizationId), ModelId(templateId))
        )
        .map(
          (_deleted: Boolean) => respond.OK(s"Deleted template [${templateId}]")
        )
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: InvalidSchemaId =>
          Future.successful(respond.NotFound(e.message))
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }
  }

  /*
   * Apply a given template to a dataset and persist this change in
   * the model service
   */
  override def applyTemplateToDataset(
    respond: TemplatesResource.applyTemplateToDatasetResponse.type
  )(
    organizationId: Int,
    templateId: String,
    datasetId: Int,
    `x-Bf-Trace-Id`: String
  ): Future[TemplatesResource.applyTemplateToDatasetResponse] =
    withAuthorization(organizationId, datasetId) { _ =>
      for {
        template <- msContainer.db.run(
          TemplateSchemaMapper
            .getSchema(OrganizationId(organizationId), ModelId(templateId))
        )
        modelId <- ModelService.createModel(
          template,
          DatasetId(datasetId),
          token,
          `x-Bf-Trace-Id`
        )
        schemaProperties <- ModelService
          .createModelPropertiesFromJSONTemplate(
            template,
            DatasetId(datasetId),
            token,
            modelId,
            `x-Bf-Trace-Id`
          )(system, executionContext, mat, msContainer)
      } yield respond.Created(schemaProperties.map(_.toDTO).toIndexedSeq)
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: InvalidSchemaId =>
          Future.successful(respond.NotFound(e.message))
        case e: InvalidModelProperties =>
          Future.successful(
            respond.BadRequest(definitions.ModelServiceBadRequest(e.message))
          )
        case e: ModelServiceJsonError =>
          Future.successful(
            respond.BadRequest(
              definitions.ModelServiceBadRequest(e.message, Some(e.error))
            )
          )
        case e: InvalidTemplateSchemaFormat =>
          Future.successful(
            respond.BadRequest(definitions.ModelServiceBadRequest(e.message))
          )
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }

  /*
   * Create a template from a given model in the ModelService
   */
  def createTemplateFromModel(
    respond: TemplatesResource.createTemplateFromModelResponse.type
  )(
    organizationId: Int,
    datasetId: Int,
    modelId: String,
    `x-Bf-Trace-Id`: String
  ): scala.concurrent.Future[
    TemplatesResource.createTemplateFromModelResponse
  ] =
    withAuthorization(organizationId, datasetId) { _ =>
      val DEFAULT_SCHEMA = "http://schema.pennsieve.net/model/draft-01/schema"

      for {
        model <- ModelService.getModel(
          ModelId(modelId),
          DatasetId(datasetId),
          token,
          `x-Bf-Trace-Id`
        )
        modelProperties <- ModelService.getModelProperties(
          ModelId(modelId),
          DatasetId(datasetId),
          token,
          `x-Bf-Trace-Id`
        )
        created <- SchemaManager.create(
          OrganizationId(organizationId),
          CreateSchemaRequest(
            schema = DEFAULT_SCHEMA,
            name = model.name,
            description = model.description.get,
            properties = modelProperties
              .map(prop => (prop.name -> prop.toSchemaProperty.toOption.get))
              .toMap
              .asJson,
            required = modelProperties
              .filter(_.required)
              .map(_.name)
              .toList
          ),
          model.templateId.map(ModelId(_))
        )

        // update model in the ModelService with the new templateId
        newTemplateId = created.id.value
        _ <- ModelService.updateModel(
          ModelId(modelId),
          DatasetId(datasetId),
          model.copy(templateId = Some(newTemplateId)).toCreateRequest,
          token,
          `x-Bf-Trace-Id`
        )
      } yield respond.Created(created.toDTO)
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: TemplateHasNotChangedException =>
          Future.successful(
            respond.BadRequest(definitions.ModelServiceBadRequest(e.message))
          )
        case e: InvalidModelId =>
          Future.successful(respond.NotFound(e.message))
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }
}

object TemplatesHandler {
  def secureRoutes(
    implicit
    system: ActorSystem,
    materializer: ActorMaterializer,
    executionContext: ExecutionContext,
    msContainer: Container
  ): Route = authenticateJwt(system.name)(msContainer.jwt) { claim: Claim =>
    TemplatesResource.routes(new TemplatesHandler(claim))
  }
}
