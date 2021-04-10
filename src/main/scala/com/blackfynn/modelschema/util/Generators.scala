package com.blackfynn.modelschema.util
import com.blackfynn.modelschema.model.{
  ModelId,
  TemplateSchemaLinkedProperty,
  TemplateSchemaRecord,
  TemplateSchemaRelationship
}
import java.time.{ OffsetDateTime, ZoneOffset }
import java.util.UUID

import com.blackfynn.modelschema.model.{
  DatasetId,
  DatasetTemplateId,
  ModelId,
  OrganizationId,
  PropertyType
}
import com.blackfynn.modelschema.model.implicits._
import com.blackfynn.modelschema.responses
import com.blackfynn.modelschema.managers.SchemaManager
import io.circe._
import io.circe.syntax._
import shapeless.{ :+:, CNil, Coproduct }
import shapeless.syntax.inject._
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

object Generators {

  def seed = Seed.random()

  val genParams: Gen.Parameters = Gen.Parameters.default

  val genBoolean: Gen[Boolean] = Gen.oneOf(Gen.const(true), Gen.const(false))

  def genOrganizationId: Gen[OrganizationId] =
    for {
      id <- Gen.chooseNum(0, 10000)
    } yield OrganizationId(id)

  def genDatasetId: Gen[DatasetId] =
    for {
      id <- Gen.chooseNum(0, 10000)
    } yield DatasetId(id)

  def genDatasetTemplateId: Gen[DatasetTemplateId] =
    for {
      id <- Gen.chooseNum(0, 10000)
    } yield DatasetTemplateId(id)

  def genModelId: Gen[ModelId] =
    for {
      id <- Gen.uuid
    } yield ModelId(id.toString)

  def genPropertyType: Gen[PropertyType] =
    Gen.oneOf(
      Seq(
        PropertyType.Boolean,
        PropertyType.Double,
        PropertyType.Long,
        PropertyType.String
      )
    )

  def genModelResponse: Gen[responses.modelservice.ModelResponse] =
    for {
      id <- Gen.uuid
      name <- Gen.identifier
      displayName = name.toUpperCase
      description <- Gen.option(Gen.identifier)
      locked <- genBoolean
      count <- Gen.chooseNum(0, 5)
      propertyCount <- Gen.chooseNum(0, 5)
      createdAt = OffsetDateTime.now(ZoneOffset.UTC)
      updatedAt = OffsetDateTime.now(ZoneOffset.UTC)
      templateId = None
    } yield
      responses.modelservice.ModelResponse(
        id = id,
        name = name,
        displayName = displayName,
        description = description,
        locked = locked,
        count = count,
        propertyCount = propertyCount,
        createdAt = createdAt,
        updatedAt = updatedAt,
        templateId = templateId
      )

  /**
    * Normalize a list of SchemaPropertyResponse instances.
    *
    * Will
    * - ensure only one prop has a conceptTitle = true
    * - will number index values correctly
    *
    * @param props
    * @return
    */
  def normalizeSchemaProperties(
    props: Seq[responses.modelservice.SchemaPropertyResponse]
  ): Seq[responses.modelservice.SchemaPropertyResponse] = {
    val allFalse = props.map { prop =>
      prop.copy(conceptTitle = false)
    }
    if (allFalse.isEmpty) {
      return allFalse
    }
    val propsP: Seq[responses.modelservice.SchemaPropertyResponse] = Seq(
      allFalse.head.copy(conceptTitle = true)
    ) ++ allFalse.tail
    propsP.zipWithIndex.map { case (prop, i) => prop.copy(index = i) }
  }

  def genSchemaPropertyResponse
    : Gen[responses.modelservice.SchemaPropertyResponse] =
    for {
      id <- Gen.uuid
      name <- Gen.identifier
      displayName = name.toUpperCase
      dataType <- genPropertyType
      locked <- genBoolean
      default <- genBoolean
      conceptTitle <- genBoolean
      description <- Gen.identifier
      required <- genBoolean
      createdAt = OffsetDateTime.now(ZoneOffset.UTC)
      updatedAt = OffsetDateTime.now(ZoneOffset.UTC)
    } yield
      responses.modelservice.SchemaPropertyResponse(
        id = id,
        name = name,
        displayName = displayName,
        dataType = dataType.asJson,
        index = 0,
        locked = locked,
        default = default,
        conceptTitle = conceptTitle,
        description = description,
        required = required,
        createdAt = createdAt,
        updatedAt = updatedAt
      )

  def genModelAndProperties(
    minProps: Int,
    maxProps: Int
  ): Gen[responses.modelservice.ModelAndProperties] =
    for {
      nProps <- Gen.chooseNum(minProps, maxProps)
      model <- genModelResponse
      properties <- Gen.listOfN(nProps, genSchemaPropertyResponse)
    } yield
      responses.modelservice
        .ModelAndProperties(
          model = model,
          properties = normalizeSchemaProperties(properties)
        )

  def genRelationshipResponse(
    maxProps: Int,
    from: Gen[Option[UUID]] = Gen.option(Gen.uuid),
    to: Gen[Option[UUID]] = Gen.option(Gen.uuid)
  ): Gen[responses.modelservice.RelationshipResponse] =
    for {
      id <- Gen.uuid
      name <- Gen.identifier
      displayName = name.toUpperCase
      description <- Gen.identifier
      nProps <- Gen.chooseNum(1, maxProps)
      schema <- Gen.listOfN(nProps, genSchemaPropertyResponse)
      from_ <- from
      to_ <- to
      createdAt = OffsetDateTime.now(ZoneOffset.UTC)
      updatedAt = OffsetDateTime.now(ZoneOffset.UTC)
    } yield
      responses.modelservice.RelationshipResponse(
        id = id,
        name = name,
        displayName = displayName,
        description = description,
        schema = normalizeSchemaProperties(schema),
        from = from_,
        to = to_,
        createdAt = createdAt,
        updatedAt = updatedAt
      )

  def genTemplateSchemaRelationship(
    maxProps: Int,
    datasetTemplateId: Gen[DatasetTemplateId] = genDatasetTemplateId,
    from: Gen[UUID] = Gen.uuid,
    to: Gen[UUID] = Gen.uuid
  ): Gen[TemplateSchemaRelationship] =
    for {
      name <- Gen.identifier
      displayName = name.toUpperCase
      description <- Gen.identifier
      nProps <- Gen.chooseNum(1, maxProps)
      schema <- Gen.listOfN(nProps, genSchemaPropertyResponse)
      from_ <- from
      to_ <- to
      datasetTemplateId <- datasetTemplateId
    } yield
      TemplateSchemaRelationship(
        name = name,
        displayName = displayName,
        description = description,
        schema = normalizeSchemaProperties(schema),
        from = Some(ModelId(from_.toString)),
        to = Some(ModelId(to_.toString)),
        datasetTemplateId = datasetTemplateId
      )

  def genLinkedPropertyResponse(
    from: Gen[UUID] = Gen.uuid,
    to: Gen[UUID] = Gen.uuid
  ): Gen[responses.modelservice.LinkedPropertyResponse] =
    for {
      id <- Gen.uuid
      name <- Gen.identifier
      displayName = name.toUpperCase
      position <- Gen.chooseNum(1, 100)
      from_ <- from
      to_ <- to
      createdAt = OffsetDateTime.now(ZoneOffset.UTC)
      updatedAt = OffsetDateTime.now(ZoneOffset.UTC)
    } yield
      responses.modelservice.LinkedPropertyResponse(
        id = id,
        name = name,
        displayName = displayName,
        position = position,
        from = from_,
        to = to_,
        createdAt = createdAt,
        updatedAt = updatedAt
      )

  def genLinkedPropertyTargetResponse(
    from: Gen[UUID] = Gen.uuid,
    to: Gen[UUID] = Gen.uuid
  ): Gen[responses.modelservice.LinkedPropertyTargetResponse] =
    for {
      link <- genLinkedPropertyResponse(from, to)
      concept <- Gen.uuid
    } yield
      responses.modelservice
        .LinkedPropertyTargetResponse(link = link, concept = concept)

  def genTemplateSchemaLinkedProperty(
    datasetTemplateId: Gen[DatasetTemplateId] = genDatasetTemplateId,
    from: Gen[UUID] = Gen.uuid,
    to: Gen[UUID] = Gen.uuid
  ): Gen[TemplateSchemaLinkedProperty] =
    for {
      name <- Gen.identifier
      displayName = name.toUpperCase
      position <- Gen.chooseNum(1, 100)
      from_ <- from
      to_ <- to
      datasetTemplateId <- datasetTemplateId
    } yield
      TemplateSchemaLinkedProperty(
        name = name,
        displayName = displayName,
        position = position,
        from = ModelId(from_.toString),
        to = ModelId(to_.toString),
        datasetTemplateId = datasetTemplateId
      )

  def genTemplateSchemaRecord(
    organizationId: Gen[OrganizationId] = genOrganizationId,
    datasetTemplateId: Gen[DatasetTemplateId] = genDatasetTemplateId
  ): Gen[TemplateSchemaRecord] =
    for {
      id <- Gen.uuid
      organizationId <- organizationId
      name <- Gen.identifier
      description <- Gen.identifier
      properties <- Gen.listOfN(5, genSchemaPropertyResponse)
      datasetTemplateId <- datasetTemplateId
    } yield
      TemplateSchemaRecord(
        id = ModelId(id.toString),
        organizationId = organizationId,
        schema = SchemaManager.validationSchemaURI,
        name = name,
        displayName = name.toUpperCase,
        description = description,
        properties = normalizeSchemaProperties(properties).asJson,
        required = properties.map(_.name),
        icon = None,
        datasetTemplateId = Some(datasetTemplateId)
      )

  /**
    * Generates a random datataset schema, where relationships and linked properties are linked to corresponding models.
    *
    * @param maxModels
    * @param maxProps
    * @param maxRelationships
    * @param maxLinkedProps
    * @return
    */
  def genDatasetSchema(
    modelBounds: (Int, Int),
    propBounds: (Int, Int),
    relationshipBounds: (Int, Int),
    linkedPropBounds: (Int, Int)
  ): Gen[responses.modelservice.DatasetSchemaResponse] = {
    val (minModels, maxModels) = modelBounds
    val (minProps, maxProps) = propBounds
    val (minRels, maxRels) = relationshipBounds
    val (minLinkedProps, maxLinkedProps) = linkedPropBounds
    for {
      nModels <- Gen.chooseNum(minModels, maxModels)
      nRelationships <- Gen.chooseNum(minRels, maxRels)
      nLinkedProps <- Gen.chooseNum(minLinkedProps, maxLinkedProps)
      modelsAndProperties <- Gen.listOfN(
        nModels,
        genModelAndProperties(minProps, maxProps)
      )
      modelIds = modelsAndProperties.map {
        case responses.modelservice.ModelAndProperties(model, _) => model.id
      }
      modelIdPicker = Gen.oneOf(modelIds)
      maybeModelIdPicker = Gen.option(modelIdPicker)
      relationships <- Gen.listOfN(
        nRelationships,
        genRelationshipResponse(
          maxProps,
          from = maybeModelIdPicker,
          to = maybeModelIdPicker
        )
      )
      linkedProperties <- Gen.listOfN(
        nLinkedProps,
        genLinkedPropertyTargetResponse(
          from = modelIdPicker,
          to = modelIdPicker
        )
      )
    } yield
      responses.modelservice.DatasetSchemaResponse(
        modelsAndProperties = modelsAndProperties,
        relationships = relationships,
        linkedProperties = linkedProperties
      )
  }

  // -------------------------------------------------------------------------------------------------------------------

  /**
    * Generate a random relationship response
    * @return
    */
  def randomRelationshipResponse(
    maxProps: Int
  ): responses.modelservice.RelationshipResponse =
    genRelationshipResponse(maxProps).pureApply(genParams, seed)

  /**
    * Generate a random TemplateSchemaRelationship
    * @return
    */
  def randomTemplateSchemaRelationship(
    maxProps: Int,
    datasetTemplateId: DatasetTemplateId = randomDatasetTemplateId,
    from: UUID = Gen.uuid.pureApply(genParams, seed),
    to: UUID = Gen.uuid.pureApply(genParams, seed)
  ): TemplateSchemaRelationship =
    genTemplateSchemaRelationship(maxProps, datasetTemplateId, from, to)
      .pureApply(genParams, seed)

  /**
    * Generate a random schema property response
    * @return
    */
  def randomSchemaPropertyResponse
    : responses.modelservice.SchemaPropertyResponse =
    genSchemaPropertyResponse.pureApply(genParams, seed)

  /**
    * Generate a random linked property response
    * @return
    */
  def randomLinkedPropertyResponse
    : responses.modelservice.LinkedPropertyResponse =
    genLinkedPropertyResponse().pureApply(genParams, seed)

  /**
    * Generate a random TemplateSchemaLinkedProperty
    * @return
    */
  def randomTemplateSchemaLinkedProperty(
    datasetTemplateId: DatasetTemplateId = randomDatasetTemplateId,
    from: UUID = Gen.uuid.pureApply(genParams, seed),
    to: UUID = Gen.uuid.pureApply(genParams, seed)
  ): TemplateSchemaLinkedProperty =
    genTemplateSchemaLinkedProperty(datasetTemplateId, from, to)
      .pureApply(genParams, seed)

  /**
    * Generate a random model ID
    * @return
    */
  def randomModelId: ModelId =
    genModelId.pureApply(genParams, seed)

  /**
    * Generate a random organization ID
    * @return
    */
  def randomOrganizationId: OrganizationId =
    genOrganizationId.pureApply(genParams, seed)

  /**
    * Generate a random dataset ID
    * @return
    */
  def randomDatasetId: DatasetId =
    genDatasetId.pureApply(genParams, seed)

  /**
    * Generate a random dataset template ID
    * @return
    */
  def randomDatasetTemplateId: DatasetTemplateId =
    genDatasetTemplateId.pureApply(genParams, seed)

  /**
    * Generate a random template schema record
    * @return
    */
  def randomTemplateSchemaRecord(
    organizationId: OrganizationId = randomOrganizationId,
    datasetTemplateId: DatasetTemplateId = randomDatasetTemplateId
  ): TemplateSchemaRecord =
    genTemplateSchemaRecord(organizationId, datasetTemplateId)
      .pureApply(genParams, seed)

  /**
    * Generate a random dataset schema
    * @param maxModels
    * @param maxProps
    * @param maxRelationships
    * @param maxLinkedProps
    * @return
    */
  def randomDatasetSchema(
    modelBounds: (Int, Int) = (3, 5),
    propBounds: (Int, Int) = (1, 5),
    relationshipBounds: (Int, Int) = (1, 5),
    linkedPropBounds: (Int, Int) = (1, 5)
  ): responses.modelservice.DatasetSchemaResponse =
    genDatasetSchema(
      modelBounds = modelBounds,
      propBounds = propBounds,
      relationshipBounds = relationshipBounds,
      linkedPropBounds = linkedPropBounds
    ).pureApply(genParams, seed)

  def randomSchemaProperties(
    nProps: Int
  ): Seq[responses.modelservice.SchemaPropertyResponse] =
    normalizeSchemaProperties(
      Gen.listOfN(nProps, genSchemaPropertyResponse).pureApply(genParams, seed)
    )
}
