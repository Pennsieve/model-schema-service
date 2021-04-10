package com.blackfynn.modelschema.managers

import java.time.{ OffsetDateTime, ZoneOffset }
import java.util.UUID

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.implicits._
import com.blackfynn.auth.middleware.{
  Jwt,
  ServiceClaim,
  UserClaim,
  DatasetId => JwtDatasetId
}
import com.blackfynn.models.Role
import com.blackfynn.modelschema.clients.{ APIService, ModelService }
import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.db.{
  DatasetTemplateMapper,
  TemplateSchemaLinkedPropertyMapper,
  TemplateSchemaMapper,
  TemplateSchemaRelationshipMapper
}
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.responses.modelservice.SchemaPropertyResponse
import com.blackfynn.modelschema.{ responses, Container }
import io.circe.Json
import io.circe.syntax._
import shapeless.syntax.inject._

import scala.concurrent.{ ExecutionContext, Future }

object DatasetTemplateManager {

  /**
    * Converts a model service model response to a storable representation used by this service,
    * overwriting the ID of the template in the process.
    *
    * @param datasetTemplateId
    * @param organizationId
    * @param model
    * @param properties
    * @return
    */
  private def toTemplateSchemaRecord(
    datasetTemplateId: DatasetTemplateId,
    organizationId: OrganizationId,
    model: responses.modelservice.ModelResponse,
    properties: Seq[responses.modelservice.SchemaPropertyResponse]
  ): TemplateSchemaRecord = {
    TemplateSchemaRecord(
      id = ModelId(UUID.randomUUID().toString),
      organizationId = organizationId,
      schema = SchemaManager.validationSchemaURI,
      name = model.name,
      displayName = model.displayName,
      description = model.description.getOrElse(model.displayName),
      category = None,
      properties = properties.asJson,
      required = List(),
      createdAt = OffsetDateTime.now(ZoneOffset.UTC),
      deleted = false,
      icon = None,
      parentId = model.templateId.map(ModelId(_)),
      datasetTemplateId = Some(datasetTemplateId)
    )
  }

  /**
    * Converts a model service relationship to a storable representation used by this service.
    *
    * @param datasetTemplateId
    * @param idIndex
    * @param relationship
    * @return
    */
  private def toTemplateSchemaRelationship(
    datasetTemplateId: DatasetTemplateId,
    idIndex: Map[UUID, ModelId]
  )(
    relationship: responses.modelservice.RelationshipResponse
  ): TemplateSchemaRelationship = {
    TemplateSchemaRelationship(
      name = relationship.name,
      displayName = relationship.displayName,
      description = relationship.description,
      schema = relationship.schema,
      from = relationship.from.map(idIndex(_)),
      to = relationship.to.map(idIndex(_)),
      datasetTemplateId = datasetTemplateId
    )
  }

  /**
    * Converts a model service linked property to a storable representation used by this service
    *
    * @param datasetTemplateId
    * @param idIndex
    * @param linkedProperty
    * @return
    */
  private def toTemplateLinkedProperty(
    datasetTemplateId: DatasetTemplateId,
    idIndex: Map[UUID, ModelId]
  )(
    linkedProperty: responses.modelservice.LinkedPropertyResponse
  ): TemplateSchemaLinkedProperty =
    TemplateSchemaLinkedProperty(
      name = linkedProperty.name,
      displayName = linkedProperty.displayName,
      position = linkedProperty.position,
      from = idIndex(linkedProperty.from),
      to = idIndex(linkedProperty.to),
      datasetTemplateId = datasetTemplateId
    )

  /**
    * Get a dataset template by ID.
    *
    * @param id
    * @param ec
    * @param msContainer
    */
  def getTemplate(
    id: DatasetTemplateId
  )(implicit
    ec: ExecutionContext,
    msContainer: Container
  ): Future[DatasetTemplate] =
    msContainer.db.run(DatasetTemplateMapper.getDatasetTemplate(id))

  /**
    * Gets the template schema for a dataset template.
    *
    * @param datasetTemplateId
    * @param ec
    * @param msContainer
    * @return
    */
  def getTemplateSchema(
    datasetTemplateId: DatasetTemplateId
  )(implicit
    ec: ExecutionContext,
    msContainer: Container
  ): Future[DatasetTemplateSchema] =
    msContainer.db.run(
      DatasetTemplateMapper
        .getDatasetTemplateSchema(datasetTemplateId)
    )

  /**
    * Given schema models, properties, relationships and linked properties, remap the IDs of the objects referencing
    * the given models.
    *
    * @param organizationId
    * @param datasetTemplateId
    * @param name
    * @param description
    * @param datasetSchema
    * @return
    */
  def remapSchemaObjects(
    organizationId: OrganizationId,
    datasetTemplateId: DatasetTemplateId,
    name: String,
    description: String
  )(
    datasetSchema: responses.modelservice.DatasetSchemaResponse
  ): DatasetTemplateSchema = {

    // 1. Convert the model responses to storable models, returning the original ID for each:
    val modelsAndIds: Seq[(UUID, TemplateSchemaRecord)] =
      datasetSchema.modelsAndProperties
        .map {
          case responses.modelservice.ModelAndProperties(model, properties) => {
            val mappedModel = toTemplateSchemaRecord(
              datasetTemplateId,
              organizationId,
              model,
              properties
            )
            (model.id, mappedModel) // (model's old UUID, new mapped model)
          }
        }

    // 2. Build an index of old model IDs -> new model IDs:
    val idIndex: Map[UUID, ModelId] = modelsAndIds.map {
      case (id: UUID, model) => (id, model.id)
    }.toMap

    // 3. Remap the IDs:
    val relationshipRemapper =
      toTemplateSchemaRelationship(datasetTemplateId, idIndex)(_)
    val linkedPropertyRemapper =
      toTemplateLinkedProperty(datasetTemplateId, idIndex)(_)

    val models: List[TemplateSchemaRecord] = modelsAndIds.map {
      case (_, model: TemplateSchemaRecord) => model
    }.toList

    val relationships: Seq[TemplateSchemaRelationship] =
      datasetSchema.relationships
        .map(relationshipRemapper)

    val linkedProperties: Seq[TemplateSchemaLinkedProperty] =
      datasetSchema.linkedProperties
        .map((res) => linkedPropertyRemapper(res.link))

    DatasetTemplateSchema(
      id = datasetTemplateId,
      name = name,
      description = description,
      models = models,
      relationships = relationships,
      linkedProperties = linkedProperties
    )
  }

  /**
    * Encode the result of saving a dataset schemal
    *
    * @param modelCount
    * @param relationshipsCount
    * @param linkedPropertyCount
    */
  case class PersistResult(
    modelCount: Long,
    relationshipsCount: Long,
    linkedPropertyCount: Long
  )

  /**
    * Persist the given dataset schema objects to the database.
    *
    * @param datasetSchema
    * @param ec
    * @param msContainer
    * @return
    */
  def persistSchemaObjects(
    datasetSchema: DatasetTemplateSchema
  )(implicit
    ec: ExecutionContext,
    msContainer: Container
  ): Future[PersistResult] = {

    val actions = for {
      m <- TemplateSchemaMapper ++= datasetSchema.models
      r <- TemplateSchemaRelationshipMapper ++= datasetSchema.relationships
      lp <- TemplateSchemaLinkedPropertyMapper ++= datasetSchema.linkedProperties
    } yield {
      val mCount: Int = (m: Option[Int]).getOrElse(0)
      val rCount: Int = r.getOrElse(0)
      val lpCount: Int = lp.getOrElse(0)
      PersistResult(mCount, rCount, lpCount)
    }

    msContainer.db.run(actions.transactionally)
  }

  private def recreateModelAndProperties(
    model: TemplateSchemaRecord,
    publishTo: DatasetId,
    token: Jwt.Token,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[((ModelId, ModelId), Seq[SchemaPropertyResponse])] = {

    for {
      newId <- ModelService.createModel(model, publishTo, token, traceId)
      properties <- ModelService.createModelPropertiesFromAPIResponse(
        model,
        publishTo,
        token,
        newId,
        traceId
      )
    } yield ((model.id, newId), properties)
  }

  case class PublishResult(
    datasetCreateResponse: Json,
    models: Map[ModelId, Seq[responses.modelservice.SchemaPropertyResponse]],
    relationships: Seq[responses.modelservice.RelationshipResponse],
    linkedProperties: Seq[responses.modelservice.LinkedPropertyResponse]
  )

  /**
    * Publish the given dataset template to a new dataset.
    *
    * @param datasetSchema
    * @param name
    * @param description
    * @param claim
    * @param system
    * @param ec
    * @param mat
    * @param msContainer
    * @return
    */
  def publishSchema(
    datasetSchema: DatasetTemplateSchema,
    name: String,
    description: Option[String],
    claim: Jwt.Claim,
    traceId: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[PublishResult] = {

    def createModelsAndProperties(
      publishTo: DatasetId,
      upgradedToken: Jwt.Token
    ) = {
      datasetSchema.models
        .map { model: TemplateSchemaRecord =>
          recreateModelAndProperties(model, publishTo, upgradedToken, traceId)
        }
        .toList
        .sequence
    }

    def createRelationships(
      publishTo: DatasetId,
      mapper: Map[ModelId, ModelId],
      upgradedToken: Jwt.Token
    ) = {
      val idMapper = (id: ModelId) => mapper(id)
      datasetSchema.relationships
        .map { relationship: TemplateSchemaRelationship =>
          {
            // We need to replace occurrences of the old model ID with that of the new result
            // from creating a new relationship through the model API:
            ModelService.createRelationship(
              relationship
                .copy(
                  from = relationship.from.map(idMapper),
                  to = relationship.to.map(idMapper)
                ),
              publishTo,
              upgradedToken,
              traceId
            )
          }
        }
        .toList
        .sequence
    }

    def createLinkedProperties(
      publishTo: DatasetId,
      mapper: Map[ModelId, ModelId],
      upgradedToken: Jwt.Token
    ) = {
      val idMapper = (id: ModelId) => mapper(id)
      datasetSchema.linkedProperties
        .map { linkedProperty: TemplateSchemaLinkedProperty =>
          {
            // We need to replace occurrences of the old model ID with that of the new result
            // from creating a new linked property through the model API:
            ModelService.createLinkedProperty(
              linkedProperty.copy(
                from = idMapper(linkedProperty.from),
                to = idMapper(linkedProperty.to)
              ),
              publishTo,
              idMapper(linkedProperty.from),
              upgradedToken,
              traceId
            )
          }
        }
        .toList
        .sequence
    }

    for {
      createResult <- APIService.createDataset(
        name,
        description,
        Jwt.generateToken(claim)(msContainer.jwt),
        traceId
      )
      (datasetCreateResponse, publishToIntId, publishToNodeId) = createResult

      datasetRole = Jwt.DatasetRole(
        id = JwtDatasetId(publishToIntId.value)
          .inject[Jwt.Role.RoleIdentifier[JwtDatasetId]],
        role = Role.Owner,
        node_id = Some(publishToNodeId)
      )
      upgradedClaim = claim.content match {
        case userClaim: UserClaim =>
          claim.copy(
            content = userClaim.copy(roles = datasetRole :: claim.content.roles)
          )
        case serviceClaim: ServiceClaim =>
          claim.copy(
            content =
              serviceClaim.copy(roles = datasetRole :: claim.content.roles)
          )
      }
      upgradedToken = Jwt.generateToken(upgradedClaim)(msContainer.jwt)

      modelsAndProps <- createModelsAndProperties(publishToIntId, upgradedToken)

      idMapper = modelsAndProps.map { case (ids, _) => ids }.toMap
      modelsToProps = modelsAndProps.map {
        case (ids, props) => (ids._2, props)
      }.toMap
      relationships <- createRelationships(
        publishToIntId,
        idMapper,
        upgradedToken
      )

      linkedProperties <- createLinkedProperties(
        publishToIntId,
        idMapper,
        upgradedToken
      )

    } yield {
      PublishResult(
        datasetCreateResponse = datasetCreateResponse,
        models = modelsToProps,
        relationships = relationships,
        linkedProperties = linkedProperties
      )
    }
  }
}
