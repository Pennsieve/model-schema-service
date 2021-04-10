package com.blackfynn.modelschema

import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{ Authorization, OAuth2BearerToken }
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.blackfynn.auth.middleware.OrganizationNodeId
import com.blackfynn.http.server.definitions
import com.blackfynn.models.Feature.DatasetTemplatesFeature
import com.blackfynn.models.Role
import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.db.{
  DatasetTemplateMapper,
  TemplateSchemaLinkedPropertyMapper,
  TemplateSchemaMapper,
  TemplateSchemaRelationshipMapper
}
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.model.implicits._
import com.blackfynn.modelschema.util.{
  Generators,
  MockHttpResponder,
  Token,
  TraceIdHeader
}
import com.blackfynn.service.utilities.MigrationRunner
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe._
import io.circe.syntax._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.concurrent.ScalaFutures

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext }

class DatasetTemplatesHandlerMockResponder(
  implicit
  val system: ActorSystem,
  val executionContext: ExecutionContext,
  val materializer: ActorMaterializer
) extends MockHttpResponder {

  val traceIdHeader = TraceIdHeader("1234-5678")

  val datasetSchema = Generators.randomDatasetSchema(modelBounds = (3, 5))
  val models = datasetSchema.modelsAndProperties.map(_.model)
  val modelSchemaProperties = datasetSchema.modelsAndProperties.map {
    case responses.modelservice.ModelAndProperties(model, properties) =>
      model.id -> properties
  }.toMap
  val modelLinkedProperties = models.map { model =>
    model.id -> datasetSchema.linkedProperties.filter {
      case responses.modelservice.LinkedPropertyTargetResponse(link, _) =>
        link.to == model.id || link.from == model.id
    }
  }.toMap

  val FeatureFlagsUri = ".*/organizations/([^/]+)$".r
  val ConceptsUri = ".*/datasets/([0-9]+)/concepts$".r
  val DatasetsUri = ".*/datasets$".r
  val RelationshipsUri = ".*/datasets/([0-9]+)/relationships$".r
  val ModelSchemaPropertiesUri =
    ".*/datasets/([0-9]+)/concepts/([^/]+)/properties$".r
  val ModelLinkedPropertiesUri =
    ".*/datasets/([0-9]+)/concepts/([^/]+)/linked$".r

  val TemplatesEnabledNodeId = "templates-enabled-node-id"
  val TemplatesDisabledNodeId = "templates-disabled-node-id"

  val publishedDatasetId = 8888
  val publishedDatasetNodeId = "published-dataset-node-id"

  // tracking
  var createdDatasets = 0
  var createdModels = 0
  var createdSchemaProperties = 0
  var createdRelationships = 0
  var createdLinkedProperties = 0

  def reset() = {
    createdDatasets = 0
    createdModels = 0
    createdSchemaProperties = 0
    createdRelationships = 0
    createdLinkedProperties = 0
  }

  override def mock = {
    // feature flags
    case (HttpMethods.GET, FeatureFlagsUri(organizationNodeId)) =>
      if (organizationNodeId == TemplatesEnabledNodeId)
        (
          StatusCodes.OK,
          Json.obj(
            (
              "organization",
              Json
                .obj(
                  (
                    "features",
                    Json.arr(Json.fromString(DatasetTemplatesFeature.entryName))
                  )
                )
            )
          )
        )
      else
        (
          StatusCodes.OK,
          Json.obj(("organization", Json.obj(("features", Json.arr()))))
        )

    // create dataset
    case (HttpMethods.POST, DatasetsUri()) => {
      createdDatasets += 1
      (
        StatusCodes.Created,
        Json.obj(
          (
            "content",
            Json.obj(
              ("intId", Json.fromInt(publishedDatasetId)),
              ("id", Json.fromString(publishedDatasetNodeId))
            )
          )
        )
      )
    }

    // create model
    case (HttpMethods.POST, ConceptsUri(datasetId))
        if datasetId.toInt == publishedDatasetId => {
      createdModels += 1
      (StatusCodes.Created, Map("id" -> Generators.randomModelId).asJson)
    }

    // create schema property
    case (HttpMethods.PUT, ModelSchemaPropertiesUri(datasetId, _))
        if datasetId.toInt == publishedDatasetId => {
      createdSchemaProperties += 1
      (
        StatusCodes.OK,
        Seq(
          Generators.randomSchemaPropertyResponse,
          Generators.randomSchemaPropertyResponse
        ).asJson
      )
    }

    // create relationship
    case (HttpMethods.POST, RelationshipsUri(datasetId))
        if datasetId.toInt == publishedDatasetId => {
      createdRelationships += 1
      (StatusCodes.Created, Generators.randomRelationshipResponse(3).asJson)
    }

    // create linked property
    case (HttpMethods.POST, ModelLinkedPropertiesUri(datasetId, _))
        if datasetId.toInt == publishedDatasetId => {
      createdLinkedProperties += 1
      (StatusCodes.Created, Generators.randomLinkedPropertyResponse.asJson)
    }

    // concepts
    case (HttpMethods.GET, ConceptsUri(datasetId))
        if List(1, 2) contains datasetId.toInt =>
      (StatusCodes.OK, datasetSchema.modelsAndProperties.map(_.model).asJson)

    // concept properties
    case (HttpMethods.GET, ModelSchemaPropertiesUri(datasetId, modelId))
        if List(1, 2) contains datasetId.toInt =>
      (StatusCodes.OK, modelSchemaProperties(UUID.fromString(modelId)).asJson)

    // relationships
    case (HttpMethods.GET, RelationshipsUri(datasetId))
        if List(1, 2) contains datasetId.toInt =>
      (StatusCodes.OK, datasetSchema.relationships.asJson)

    // linked properties
    case (HttpMethods.GET, ModelLinkedPropertiesUri(datasetId, modelId))
        if List(1, 2) contains datasetId.toInt =>
      (StatusCodes.OK, modelLinkedProperties(UUID.fromString(modelId)).asJson)

    case (method, uri) =>
      (
        StatusCodes.EnhanceYourCalm,
        s"Unexpected request to the mock concept service: $method: $uri".asJson
      )
  }
}

class DatasetTemplatesHandlerSpec
    extends HelperSpec
    with BeforeAndAfterEach
    with ScalaFutures {

  val traceIdHeader = TraceIdHeader("1234-5678")

  override val responder =
    new DatasetTemplatesHandlerMockResponder()(
      system,
      executionContext,
      materializer
    )

  lazy val routes = Route.seal(DatasetTemplatesHandler.secureRoutes)

  override def afterStart(): Unit = {
    super.afterStart

    val migrator = new MigrationRunner(
      container.jdbcUrl,
      container.username,
      container.password
    )
    val (count, _) = migrator.run()
    assert(count > 0)
  }

  override def afterEach = {
    super.afterEach()

    responder.reset()

    Await.result(
      msContainer.db.run(
        DBIO.seq(
          TemplateSchemaLinkedPropertyMapper.delete,
          TemplateSchemaRelationshipMapper.delete,
          TemplateSchemaMapper.delete,
          DatasetTemplateMapper.delete
        )
      ),
      5.seconds
    )
  }

  def addRecord(
    organizationId: OrganizationId,
    datasetId: DatasetId,
    name: String
  ): DatasetTemplateSchema = {
    val record = Await
      .result(
        msContainer.db.run(
          DatasetTemplateMapper.insert(
            DatasetTemplate(
              organizationId = organizationId,
              datasetId = datasetId,
              name = name,
              description = s"example template"
            )
          )
        ),
        5.seconds
      )

    val templateSchemas = Seq(
      addTemplateSchemaRecord(organizationId, record.id),
      addTemplateSchemaRecord(organizationId, record.id)
    )
    val linkedProperties = Seq(
      addLinkedProperty(
        templateSchemas.head.id,
        templateSchemas.last.id,
        record.id
      )
    )
    val relationships = Seq(
      addRelationship(
        templateSchemas.head.id,
        templateSchemas.last.id,
        record.id
      ),
      addRelationship(
        templateSchemas.head.id,
        templateSchemas.last.id,
        record.id
      )
    )

    DatasetTemplateSchema(
      record.id,
      record.name,
      record.description,
      templateSchemas,
      relationships,
      linkedProperties
    )
  }

  def addTemplateSchemaRecord(
    organizationId: OrganizationId,
    datasetTemplateId: DatasetTemplateId
  ): TemplateSchemaRecord = {
    val record =
      Generators.randomTemplateSchemaRecord(organizationId, datasetTemplateId)
    Await.result(msContainer.db.run(TemplateSchemaMapper += record), 5.seconds)
    record
  }

  def addLinkedProperty(
    fromId: ModelId,
    toId: ModelId,
    datasetTemplateId: DatasetTemplateId
  ): TemplateSchemaLinkedProperty = {
    val linkedProperty = Generators.randomTemplateSchemaLinkedProperty(
      datasetTemplateId,
      fromId.toUUID,
      toId.toUUID
    )
    Await.result(
      msContainer.db
        .run(TemplateSchemaLinkedPropertyMapper.insert(linkedProperty)),
      5.seconds
    )
  }

  def addRelationship(
    fromId: ModelId,
    toId: ModelId,
    datasetTemplateId: DatasetTemplateId
  ): TemplateSchemaRelationship = {
    val relationship =
      Generators.randomTemplateSchemaRelationship(
        5,
        datasetTemplateId,
        fromId.toUUID,
        toId.toUUID
      )
    Await.result(
      msContainer.db.run(TemplateSchemaRelationshipMapper.insert(relationship)),
      5.seconds
    )
  }

  "datasetTemplateRoutes GET /organizations/:org_id/dataset-templates" should {
    "return list of records when successful" in {
      // (should not get returned)
      addRecord(OrganizationId(1), DatasetId(1), "name1")

      val r1 = addRecord(OrganizationId(5), DatasetId(1), "name2")
      val r2 = addRecord(OrganizationId(5), DatasetId(1), "name3")
      val r3 = addRecord(OrganizationId(5), DatasetId(1), "name4")
      val token = Token.generate(
        OrganizationId(5),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Get("/organizations/5/dataset-templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        val result = entityAs[List[definitions.DatasetTemplate]]
          .map(rec => (rec.id, rec.name, rec.description))
        val expected =
          List(r1, r2, r3)
            .map(_.toDTO)
            .map(rec => (rec.id, rec.name, rec.description))
        result should contain theSameElementsAs expected
      }
    }

    "return empty list when organization has no templates" in {
      val token = Token.generate(
        OrganizationId(10),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Get("/organizations/10/dataset-templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)

        entityAs[List[Json]] should ===(List.empty)
      }
    }

    "return forbidden when requesting for an unauthorized organization" in {
      val token = Token.generate(
        OrganizationId(2),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Get("/organizations/1/dataset-templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "return forbidden when requesting for an organization without the feature flag" in {
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesDisabledNodeId))
      )(msContainer.jwt)
      val request = Get("/organizations/1/dataset-templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "return forbidden when request contains a missing Authorization header" in {
      val request = Get("/organizations/1/dataset-templates")

      request ~> routes ~> check {
        status should ===(StatusCodes.Unauthorized)
      }
    }
  }

  "datasetTemplateRoutes GET /organizations/:org_id/dataset-templates/:id" should {
    "return record when successful" in {
      val record = addRecord(OrganizationId(1), DatasetId(1), "name1")

      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request =
        Get(s"/organizations/1/dataset-templates/${record.id.value}")
          .addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        entityAs[definitions.DatasetTemplateSchema] should ===(record.toDTO)
      }
    }

    "return not found when id is invalid" in {
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Get("/organizations/1/templates/template-invalid")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return not found when id is associated with a different organization" in {
      val token = Token.generate(
        OrganizationId(2),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Get("/organizations/2/dataset-templates/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return forbidden when requesting for an unauthorized organization" in {
      val token = Token.generate(
        OrganizationId(2),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Get("/organizations/1/dataset-templates/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "return forbidden when requesting for an organization without the feature flag" in {
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesDisabledNodeId))
      )(msContainer.jwt)
      val request = Get("/organizations/1/dataset-templates/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }
  }

  "datasetTemplateRoutes PUT /organizations/:org_id/dataset-templates/:id" should {
    "update just the name" in {
      val record = addRecord(OrganizationId(1), DatasetId(1), "old name")
      val updateRequest =
        definitions.DatasetTemplateUpdateRequest(name = Some("new name"))

      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request =
        Put(
          s"/organizations/1/dataset-templates/${record.id.value}",
          HttpEntity(
            ContentTypes.`application/json`,
            updateRequest.asJson.noSpaces
          )
        ).addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)

        val result = Await.result(
          msContainer.db.run(
            DatasetTemplateMapper
              .getDatasetTemplateSchema(record.id)
          ),
          5.seconds
        )
        result.id shouldBe record.id
        result.name shouldBe "new name"
        result.description shouldBe record.description
        result.models should contain theSameElementsAs record.models
        result.relationships should contain theSameElementsAs record.relationships
        result.linkedProperties should contain theSameElementsAs record.linkedProperties
      }
    }

    "update just the description" in {
      val record = addRecord(OrganizationId(1), DatasetId(1), "name")
      val updateRequest =
        definitions.DatasetTemplateUpdateRequest(
          description = Some("new description")
        )

      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request =
        Put(
          s"/organizations/1/dataset-templates/${record.id.value}",
          HttpEntity(
            ContentTypes.`application/json`,
            updateRequest.asJson.noSpaces
          )
        ).addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)

        val result = Await.result(
          msContainer.db.run(
            DatasetTemplateMapper
              .getDatasetTemplateSchema(record.id)
          ),
          5.seconds
        )

        result.id shouldBe record.id
        result.name shouldBe record.name
        result.description shouldBe "new description"
        result.models should contain theSameElementsAs record.models
        result.relationships should contain theSameElementsAs record.relationships
        result.linkedProperties should contain theSameElementsAs record.linkedProperties
      }
    }

    "update both the description and the name" in {
      val record = addRecord(OrganizationId(1), DatasetId(1), "old name")
      val updateRequest =
        definitions.DatasetTemplateUpdateRequest(
          name = Some("new name"),
          description = Some("new description")
        )

      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request =
        Put(
          s"/organizations/1/dataset-templates/${record.id.value}",
          HttpEntity(
            ContentTypes.`application/json`,
            updateRequest.asJson.noSpaces
          )
        ).addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)

        val result = Await.result(
          msContainer.db.run(
            DatasetTemplateMapper
              .getDatasetTemplateSchema(record.id)
          ),
          5.seconds
        )

        result.id shouldBe record.id
        result.name shouldBe "new name"
        result.description shouldBe "new description"
        result.models should contain theSameElementsAs record.models
        result.relationships should contain theSameElementsAs record.relationships
        result.linkedProperties should contain theSameElementsAs record.linkedProperties
      }
    }

    "update neither if neither are provided" in {
      val record = addRecord(OrganizationId(1), DatasetId(1), "name")
      val updateRequest = definitions.DatasetTemplateUpdateRequest()

      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request =
        Put(
          s"/organizations/1/dataset-templates/${record.id.value}",
          HttpEntity(
            ContentTypes.`application/json`,
            updateRequest.asJson.noSpaces
          )
        ).addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)

        val result = Await.result(
          msContainer.db.run(
            DatasetTemplateMapper
              .getDatasetTemplateSchema(record.id)
          ),
          5.seconds
        )

        result.id shouldBe record.id
        result.name shouldBe record.name
        result.description shouldBe record.description
        result.models should contain theSameElementsAs record.models
        result.relationships should contain theSameElementsAs record.relationships
        result.linkedProperties should contain theSameElementsAs record.linkedProperties
      }
    }

    "return not found when id is invalid" in {
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Put(
        "/organizations/1/templates/template-invalid",
        HttpEntity(ContentTypes.`application/json`, "{}")
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return not found when id is associated with a different organization" in {
      val token = Token.generate(
        OrganizationId(2),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Put(
        "/organizations/2/dataset-templates/1",
        HttpEntity(ContentTypes.`application/json`, "{}")
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return forbidden when requesting for an unauthorized organization" in {
      val token = Token.generate(
        OrganizationId(2),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Put(
        "/organizations/1/dataset-templates/1",
        HttpEntity(ContentTypes.`application/json`, "{}")
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "return forbidden when requesting for an organization without the feature flag" in {
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesDisabledNodeId))
      )(msContainer.jwt)
      val request = Put(
        "/organizations/1/dataset-templates/1",
        HttpEntity(ContentTypes.`application/json`, "{}")
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }
  }

  "datasetTemplateRoutes DELETE /organizations/:org_id/dataset-templates/:id" should {
    "delete the dataset template if it exists" in {
      val record = addRecord(OrganizationId(1), DatasetId(1), "old name")
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request =
        Delete(s"/organizations/1/dataset-templates/${record.id.value}")
          .addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)

        // the template should no longer exist
        an[InvalidDatasetTemplateId] should be thrownBy Await.result(
          msContainer.db.run(
            DatasetTemplateMapper
              .getDatasetTemplateSchema(record.id)
          ),
          5.seconds
        )
      }
    }

    "delete the dataset template and all dependencies if it exists" in {
      val record = addRecord(OrganizationId(1), DatasetId(1), "old name")
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)

      // there should be schemas beforehand
      Await.result(
        msContainer.db.run(
          TemplateSchemaMapper
            .getSchemasForDatasetTemplate(record.id)
            .result
        ),
        5.seconds
      ) should not be empty

      // there should be relationships beforehand
      Await.result(
        msContainer.db.run(
          TemplateSchemaRelationshipMapper
            .getRelationshipsForDatasetTemplate(record.id)
            .result
        ),
        5.seconds
      ) should not be empty

      // there should be linked properties beforehand
      Await.result(
        msContainer.db.run(
          TemplateSchemaLinkedPropertyMapper
            .getLinkedPropertiesForDatasetTemplate(record.id)
            .result
        ),
        5.seconds
      ) should not be empty

      val request =
        Delete(s"/organizations/1/dataset-templates/${record.id.value}")
          .addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)

        // the template should no longer exist
        an[InvalidDatasetTemplateId] should be thrownBy Await.result(
          msContainer.db.run(
            DatasetTemplateMapper
              .getDatasetTemplateSchema(record.id)
          ),
          5.seconds
        )

        // there should be no schemas
        Await.result(
          msContainer.db.run(
            TemplateSchemaMapper
              .getSchemasForDatasetTemplate(record.id)
              .result
          ),
          5.seconds
        ) shouldBe empty

        // there should be no relationships
        Await.result(
          msContainer.db.run(
            TemplateSchemaRelationshipMapper
              .getRelationshipsForDatasetTemplate(record.id)
              .result
          ),
          5.seconds
        ) shouldBe empty

        // there should be no linked properties
        Await.result(
          msContainer.db.run(
            TemplateSchemaLinkedPropertyMapper
              .getLinkedPropertiesForDatasetTemplate(record.id)
              .result
          ),
          5.seconds
        ) shouldBe empty
      }
    }

    "return not found when id is invalid" in {
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Delete("/organizations/1/templates/template-invalid")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return not found when id is associated with a different organization" in {
      val token = Token.generate(
        OrganizationId(2),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Delete("/organizations/2/dataset-templates/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return forbidden when requesting for an unauthorized organization" in {
      val token = Token.generate(
        OrganizationId(2),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)
      val request = Delete("/organizations/1/dataset-templates/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "return forbidden when requesting for an organization without the feature flag" in {
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesDisabledNodeId))
      )(msContainer.jwt)
      val request = Delete("/organizations/1/dataset-templates/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }
  }

  "datasetTemplateRoutes POST /organizations/:org_id/datasets-templates/:dataset_template_id" should {
    "create new dataset and dependencies" in {
      val record =
        addRecord(OrganizationId(1), DatasetId(1), "existing template")

      val token =
        Token.generate(
          OrganizationId(1),
          organizationNodeId =
            Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
        )(msContainer.jwt)

      val request = Post(
        s"/organizations/1/dataset-templates/${record.id.value}",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val response = entityAs[Json]
        val createdDatasetId = response.hcursor
          .downField("content")
          .downField("intId")
          .as[Int]
          .map(DatasetId)
          .right
          .get

        createdDatasetId.value shouldBe responder.publishedDatasetId

        responder.createdDatasets shouldBe 1
        responder.createdModels shouldBe record.models.length
        responder.createdSchemaProperties shouldBe record.models.length
        responder.createdRelationships shouldBe record.relationships.length
        responder.createdLinkedProperties shouldBe record.linkedProperties.length
      }
    }

    "be forbidden without org access" in {
      val token =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
        )(msContainer.jwt)

      val request = Post(
        "/organizations/2/dataset-templates/1",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be forbidden without the feature flag" in {
      val token =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesDisabledNodeId))
        )(msContainer.jwt)

      val request = Post(
        "/organizations/1/dataset-templates/1",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "return not found if dataset template does not exist" in {
      val token =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
        )(msContainer.jwt)

      val request = Post(
        s"/organizations/1/dataset-templates/1",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }
  }

  "datasetTemplateRoutes POST /organizations/:org_id/datasets/:dataset_id/dataset-templates" should {
    "create new dataset template and dependencies" in {
      val token =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesEnabledNodeId)),
          datasetId = Some(DatasetId(1))
        )(msContainer.jwt)

      val request = Post(
        "/organizations/1/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val response: definitions.DatasetTemplate =
          entityAs[definitions.DatasetTemplate]
        val createdDataset = Await.result(
          msContainer.db.run(
            DatasetTemplateMapper
              .getDatasetTemplateSchema(DatasetTemplateId(response.id))
          ),
          5.seconds
        )
        createdDataset.name shouldBe "ds_name"
        createdDataset.description shouldBe "ds_description"

        createdDataset.models.length shouldBe (responder.models.length)
        createdDataset.relationships.length shouldBe (responder.datasetSchema.relationships.length)
        createdDataset.linkedProperties.length shouldBe (responder.datasetSchema.linkedProperties.length)
      }
    }

    "be forbidden without org access" in {
      val token =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesEnabledNodeId)),
          datasetId = Some(DatasetId(1))
        )(msContainer.jwt)

      val request = Post(
        "/organizations/2/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be forbidden without dataset access" in {
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId))
      )(msContainer.jwt)

      val request = Post(
        "/organizations/1/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be forbidden with insufficient dataset access" in {
      val token = Token.generate(
        OrganizationId(1),
        Some(OrganizationNodeId(responder.TemplatesEnabledNodeId)),
        datasetId = Some(DatasetId(1)),
        datasetRole = Role.Viewer
      )(msContainer.jwt)

      val request = Post(
        "/organizations/1/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be forbidden with no dataset access" in {
      val token =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesEnabledNodeId)),
          datasetId = Some(DatasetId(1))
        )(msContainer.jwt)

      val request = Post(
        "/organizations/1/datasets/99/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be forbidden without the feature flag" in {
      val token =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesDisabledNodeId)),
          datasetId = Some(DatasetId(1))
        )(msContainer.jwt)

      val request = Post(
        "/organizations/1/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "ds_name", "description": "ds_description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "prevent duplicate named templates from being created" in {
      val token =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesEnabledNodeId)),
          datasetId = Some(DatasetId(1))
        )(msContainer.jwt)

      val request1 = Post(
        "/organizations/1/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "non_unique_name", "description": "This is a unique description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      val request2 = Post(
        "/organizations/1/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "non_unique_name", "description": "This is definitely unique, @#$!"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request1 ~> routes ~> check {
        status should ===(StatusCodes.Created)
      }
      request2 ~> routes ~> check {
        status should ===(StatusCodes.Conflict)
      }
    }

    "allow duplicate template names after deletion" in {
      val token =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesEnabledNodeId)),
          datasetId = Some(DatasetId(1))
        )(msContainer.jwt)

      val createRequest1 = Post(
        "/organizations/1/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "non_unique_name", "description": "This is a unique description"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      val createRequest2 = Post(
        "/organizations/1/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "non_unique_name", "description": "This is definitely unique, @#$!"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      val datasetTemplate
        : definitions.DatasetTemplate = createRequest1 ~> routes ~> check {
        status should ===(StatusCodes.Created)
        entityAs[definitions.DatasetTemplate]
      }

      // Delete it:
      val deleteRequest =
        Delete(
          s"/organizations/1/dataset-templates/${datasetTemplate.id.value}"
        ).addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      deleteRequest ~> routes ~> check {
        status should ===(StatusCodes.OK)
      }

      // Should succeed:
      createRequest2 ~> routes ~> check {
        status should ===(StatusCodes.Created)
      }
    }

    "duplicate names across different organizations is ok" in {
      val tokenOrg1 =
        Token.generate(
          OrganizationId(1),
          Some(OrganizationNodeId(responder.TemplatesEnabledNodeId)),
          datasetId = Some(DatasetId(1))
        )(msContainer.jwt)

      val tokenOrg2 =
        Token.generate(
          OrganizationId(2),
          Some(OrganizationNodeId(responder.TemplatesEnabledNodeId)),
          datasetId = Some(DatasetId(2))
        )(msContainer.jwt)

      val request1 = Post(
        "/organizations/1/datasets/1/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "non_unique_name", "description": "ORGANIZATION-1"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(tokenOrg1.value)))
        .addHeader(traceIdHeader)

      val request2 = Post(
        "/organizations/2/datasets/2/dataset-templates",
        HttpEntity(
          ContentTypes.`application/json`,
          """{"name": "non_unique_name", "description": "ORGANIZATION-2"}"""
        )
      ).addHeader(Authorization(OAuth2BearerToken(tokenOrg2.value)))
        .addHeader(traceIdHeader)

      request1 ~> routes ~> check {
        status should ===(StatusCodes.Created)
      }
      request2 ~> routes ~> check {
        status should ===(StatusCodes.Created)
      }
    }
  }
}
