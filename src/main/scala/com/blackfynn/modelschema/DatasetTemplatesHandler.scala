package com.blackfynn.modelschema

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.blackfynn.auth.middleware.AkkaDirective._
import com.blackfynn.auth.middleware.Jwt.Claim
import com.blackfynn.auth.middleware.{
  DatasetPermission,
  Jwt,
  OrganizationNodeId,
  Validator,
  DatasetId => JwtDatasetId,
  OrganizationId => JwtOrganizationId
}
import com.blackfynn.http.server.datasetTemplates.{
  DatasetTemplatesResource,
  DatasetTemplatesHandler => GuardrailHandler
}
import com.blackfynn.http.server.definitions
import com.blackfynn.models.Feature.DatasetTemplatesFeature
import com.blackfynn.modelschema.clients.{ APIService, ModelService }
import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.db.{
  DatasetTemplateMapper,
  TemplateSchemaLinkedPropertyMapper,
  TemplateSchemaMapper,
  TemplateSchemaRelationshipMapper
}
import com.blackfynn.modelschema.managers.DatasetTemplateManager
import com.blackfynn.modelschema.model.{ DatasetId, _ }
import shapeless.syntax.inject._

import scala.concurrent.{ ExecutionContext, Future }

/**
  * Operations on dataset templates
  */
class DatasetTemplatesHandler(
  claim: Claim
)(implicit
  system: ActorSystem,
  mat: ActorMaterializer,
  executionContext: ExecutionContext,
  msContainer: Container
) extends GuardrailHandler {

  private val token: Jwt.Token = Jwt.generateToken(claim)(msContainer.jwt)

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
        DatasetPermission.ManageDatasetTemplates
      ))
      Future.unit
    else
      Future.failed(ModelSchemaForbiddenDatasetException(datasetId))

  /*
   * Ensure that this org has the required feature flag for dataset templates
   */
  def hasFeatureFlag(
    organizationId: Int,
    `x-Bf-Trace-Id`: String
  ): Future[Unit] = {
    val exception = ModelSchemaMissingFeatureFlagException(
      organizationId,
      DatasetTemplatesFeature
    )
    val maybeNodeId: Option[OrganizationNodeId] = claim.content.roles
      .find {
        case Jwt.OrganizationRole(id, _, _, _, _) =>
          id == JwtOrganizationId(organizationId)
            .inject[Jwt.Role.RoleIdentifier[JwtOrganizationId]]
        case _ => false
      }
      .flatMap {
        case Jwt.OrganizationRole(_, _, _, nodeId, _) => nodeId
        case _ => None
      }

    for {
      nodeId <- maybeNodeId.fold(Future.failed[OrganizationNodeId](exception))(
        Future.successful
      )
      flags <- APIService.getFeatureFlags(nodeId, token, `x-Bf-Trace-Id`)
      _ <- if (flags.contains(DatasetTemplatesFeature))
        Future.successful(())
      else Future.failed(exception)
    } yield ()
  }

  def withAuthorization[T](
    organizationId: Int,
    `x-Bf-Trace-Id`: String
  )(
    f: => Future[T]
  ): Future[T] =
    for {
      _ <- hasOrganizationAccess(organizationId)
      _ <- hasFeatureFlag(organizationId, `x-Bf-Trace-Id`)
      result <- f
    } yield result

  def withAuthorization[T](
    organizationId: Int,
    datasetId: Int,
    `x-Bf-Trace-Id`: String
  )(
    f: => Future[T]
  ): Future[T] =
    for {
      _ <- hasOrganizationAccess(organizationId)
      _ <- hasFeatureFlag(organizationId, `x-Bf-Trace-Id`)
      _ <- hasDatasetAccess(datasetId)
      result <- f
    } yield result

  /*
   * Get the dataset templates for an organization
   */
  override def getDatasetTemplatesForOrganization(
    respond: DatasetTemplatesResource.getDatasetTemplatesForOrganizationResponse.type
  )(
    organizationId: Int,
    `x-Bf-Trace-Id`: String
  ): Future[
    DatasetTemplatesResource.getDatasetTemplatesForOrganizationResponse
  ] =
    withAuthorization(organizationId, `x-Bf-Trace-Id`) {
      msContainer.db
        .run(
          DatasetTemplateMapper
            .getDatasetTemplatesForOrganization(OrganizationId(organizationId))
            .map { templates =>
              respond.OK(templates.map(_.toDTO).toIndexedSeq)
            }
        )
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }

  /*
   * Get a dataset template by org ID and template ID
   */
  override def getDatasetTemplate(
    respond: DatasetTemplatesResource.getDatasetTemplateResponse.type
  )(
    organizationId: Int,
    datasetTemplateId: Int,
    `x-Bf-Trace-Id`: String
  ): Future[DatasetTemplatesResource.getDatasetTemplateResponse] =
    withAuthorization(organizationId, `x-Bf-Trace-Id`) {
      msContainer.db
        .run(
          DatasetTemplateMapper
            .getDatasetTemplateSchema(DatasetTemplateId(datasetTemplateId))
        )
        .map((tsr: DatasetTemplateSchema) => respond.OK(tsr.toDTO))
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: InvalidDatasetTemplateId =>
          Future.successful(respond.NotFound(e.message))
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }

  /*
   * Update a dataset template
   */
  override def updateDatasetTemplate(
    respond: DatasetTemplatesResource.updateDatasetTemplateResponse.type
  )(
    organizationId: Int,
    datasetTemplateId: Int,
    datasetTemplateUpdate: definitions.DatasetTemplateUpdateRequest,
    `x-Bf-Trace-Id`: String
  ): Future[DatasetTemplatesResource.updateDatasetTemplateResponse] =
    withAuthorization(organizationId, `x-Bf-Trace-Id`) {
      msContainer.db
        .run((for {
          _ <- DatasetTemplateMapper.getDatasetTemplateSchema(
            DatasetTemplateId(datasetTemplateId)
          )
          _ <- datasetTemplateUpdate.name match {
            case Some(name) =>
              DatasetTemplateMapper.updateDatasetTemplateName(
                DatasetTemplateId(datasetTemplateId),
                name
              )
            case None => DBIO.successful(())
          }
          _ <- datasetTemplateUpdate.description match {
            case Some(description) =>
              DatasetTemplateMapper.updateDatasetTemplateDescription(
                DatasetTemplateId(datasetTemplateId),
                description
              )
            case None => DBIO.successful(())
          }
        } yield ()).transactionally)
        .map(_ => respond.Created)
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: InvalidDatasetTemplateId =>
          Future.successful(respond.NotFound(e.message))
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }

  /*
   * Delete a dataset template
   */
  override def deleteDatasetTemplate(
    respond: DatasetTemplatesResource.deleteDatasetTemplateResponse.type
  )(
    organizationId: Int,
    datasetTemplateId: Int,
    `x-Bf-Trace-Id`: String
  ): Future[DatasetTemplatesResource.deleteDatasetTemplateResponse] =
    withAuthorization(organizationId, `x-Bf-Trace-Id`) {
      msContainer.db
        .run((for {
          _ <- TemplateSchemaMapper.deleteDatasetTemplateSchemas(
            DatasetTemplateId(datasetTemplateId)
          )
          _ <- TemplateSchemaRelationshipMapper
            .deleteDatasetTemplateRelationships(
              DatasetTemplateId(datasetTemplateId)
            )
          _ <- TemplateSchemaLinkedPropertyMapper
            .deleteDatasetTemplateLinkedProperties(
              DatasetTemplateId(datasetTemplateId)
            )
          _ <- DatasetTemplateMapper.deleteDatasetTemplate(
            DatasetTemplateId(datasetTemplateId)
          )
        } yield ()).transactionally)
        .map(_ => respond.OK)
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: InvalidDatasetTemplateId =>
          Future.successful(respond.NotFound(e.message))
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }

  /*
   * Create a dataset template from a given dataset
   */
  def createTemplateFromDataset(
    respond: DatasetTemplatesResource.createTemplateFromDatasetResponse.type
  )(
    organizationId: Int,
    datasetId: Int,
    templateProperties: definitions.DatasetTemplateCreateRequest,
    `x-Bf-Trace-Id`: String
  ): scala.concurrent.Future[
    DatasetTemplatesResource.createTemplateFromDatasetResponse
  ] =
    withAuthorization(organizationId, datasetId, `x-Bf-Trace-Id`) {
      for {
        datasetSchemaObjects <- ModelService.ingestDatasetSchema(
          DatasetId(datasetId),
          token,
          `x-Bf-Trace-Id`
        )
        datasetTemplate <- msContainer.db
          .run(for {
            _ <- DatasetTemplateMapper.enforceNameUniqueness(
              OrganizationId(organizationId),
              DatasetId(datasetId),
              templateProperties.name
            )
            newTemplate <- DatasetTemplateMapper.insert(
              DatasetTemplate(
                OrganizationId(organizationId),
                DatasetId(datasetId),
                templateProperties.name,
                templateProperties.description
              )
            )
          } yield newTemplate)
        datasetSchema = DatasetTemplateManager.remapSchemaObjects(
          OrganizationId(organizationId),
          datasetTemplate.id,
          datasetTemplate.name,
          datasetTemplate.description
        )(datasetSchemaObjects)
        _ <- DatasetTemplateManager.persistSchemaObjects(datasetSchema)
      } yield respond.Created(datasetTemplate.toDTO)
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: DuplicateDatasetTemplateName =>
          Future.successful(
            respond.Conflict(
              definitions.DuplicateDatasetTemplateName(
                e.message,
                e.name,
                e.datasetId.value
              )
            )
          )
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }

  /*
   * Create a dataset from a given dataset template
   */
  def createDatasetFromTemplate(
    respond: DatasetTemplatesResource.createDatasetFromTemplateResponse.type
  )(
    organizationId: Int,
    datasetTemplateId: Int,
    datasetProperties: definitions.DatasetCreateRequest,
    `x-Bf-Trace-Id`: String
  ): scala.concurrent.Future[
    DatasetTemplatesResource.createDatasetFromTemplateResponse
  ] =
    withAuthorization(organizationId, `x-Bf-Trace-Id`) {
      for {
        datasetTemplate <- msContainer.db
          .run(
            DatasetTemplateMapper
              .getDatasetTemplateSchema(DatasetTemplateId(datasetTemplateId))
          )

        publishResult <- DatasetTemplateManager.publishSchema(
          datasetTemplate,
          datasetProperties.name,
          datasetProperties.description,
          claim,
          `x-Bf-Trace-Id`
        )
      } yield respond.Created(publishResult.datasetCreateResponse)
    }.recoverWith {
      ErrorResponse.recoverWithFn {
        case e: InvalidDatasetTemplateId =>
          Future.successful(respond.NotFound(e.message))
        case e: ModelSchemaForbiddenException =>
          Future.successful(respond.Forbidden(e.message))
      }
    }
}

object DatasetTemplatesHandler {
  def secureRoutes(
    implicit
    system: ActorSystem,
    mat: ActorMaterializer,
    executionContext: ExecutionContext,
    msContainer: Container
  ) = authenticateJwt(system.name)(msContainer.jwt) { claim =>
    DatasetTemplatesResource.routes(new DatasetTemplatesHandler(claim))
  }
}
