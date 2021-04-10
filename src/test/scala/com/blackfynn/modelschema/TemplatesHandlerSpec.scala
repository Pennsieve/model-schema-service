package com.blackfynn.modelschema

import java.util.UUID

import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{
  Authorization,
  OAuth2BearerToken,
  RawHeader
}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.Unmarshaller._
import akka.stream.ActorMaterializer
import com.blackfynn.models.Role
import com.blackfynn.concepts.models.Icon
import com.blackfynn.http.server.definitions
import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.db.{
  DatasetTemplateMapper,
  TemplateSchemaMapper
}
import com.blackfynn.modelschema.managers._
import com.blackfynn.modelschema.model.implicits._
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.util.{
  Generators,
  MockHttpResponder,
  Token,
  TraceIdHeader
}
import com.blackfynn.modelschema.clients._
import com.blackfynn.service.utilities.MigrationRunner
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.unmarshalling.Unmarshaller._
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import io.circe._
import shapeless.syntax.inject._

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext }
import org.scalatest.concurrent.ScalaFutures

class TemplatesHandlerMockResponder(
  implicit
  val system: ActorSystem,
  val executionContext: ExecutionContext,
  val materializer: ActorMaterializer
) extends MockHttpResponder {

  final val mockConceptID1 = UUID.randomUUID()
  final val mockConceptID2 = UUID.randomUUID()
  final val mockConceptID3 = UUID.randomUUID()
  final val mockConceptID4 = UUID.randomUUID()
  final val mockSchemaPropertyID1 = UUID.randomUUID()
  final val mockSchemaPropertyID2 = UUID.randomUUID()

  val okConcept1 = responses.modelservice.ModelResponse(
    id = mockConceptID1,
    name = "mock-concept-1",
    displayName = "Mock-Concept-1",
    description = Some("Just a mocked concept..."),
    locked = false,
    count = 1,
    propertyCount = 2
  )

  val okConcept2 = responses.modelservice.ModelResponse(
    id = mockConceptID2,
    name = "mock-concept-2",
    displayName = "Mock-Concept-2",
    description = Some("Another mocked concept..."),
    locked = false,
    count = 1,
    propertyCount = 2
  )

  val okConceptWithParent = responses.modelservice.ModelResponse(
    id = mockConceptID3,
    name = "mock-concept-parent",
    displayName = "Mock-Concept-parent",
    description = Some("A mocked concept that already has a template ID"),
    locked = false,
    count = 1,
    propertyCount = 2,
    templateId = Some("template1")
  )

  val schemaProperty1 = responses.modelservice.SchemaPropertyResponse(
    id = mockSchemaPropertyID1,
    name = "mock-schema-property-1",
    displayName = "Mock-Schema-Property-1",
    dataType = (PropertyType.String: PropertyType).asJson,
    index = 0,
    locked = true,
    default = true,
    conceptTitle = true,
    description = "A concept property",
    required = true
  )

  override def mock = {
    case (HttpMethods.POST, uri)
        if uri.toString.endsWith("/datasets/1/concepts") =>
      (StatusCodes.Created, okConcept1.asJson)
    case (HttpMethods.PUT, uri)
        if uri.toString.endsWith(
          s"/datasets/1/concepts/${mockConceptID1}/properties"
        ) =>
      (StatusCodes.OK, Seq(schemaProperty1).asJson)
    case (_, uri) if uri.toString.endsWith("/datasets/2/concepts") =>
      (StatusCodes.BadRequest, "bad format".asJson)
    case (HttpMethods.POST, uri)
        if uri.toString.endsWith("/datasets/3/concepts") =>
      (StatusCodes.Created, okConcept2.asJson)
    case (_, uri)
        if uri.toString.endsWith(
          s"/datasets/3/concepts/${mockConceptID2}/properties"
        ) =>
      (StatusCodes.BadRequest, "too many titles".asJson)
    case (HttpMethods.GET, uri)
        if uri.toString.endsWith(s"/datasets/1/concepts/model99") =>
      (StatusCodes.NotFound, "Model not found".asJson)
    case (HttpMethods.GET, uri)
        if uri.toString.endsWith(s"/datasets/1/concepts/model1") =>
      (StatusCodes.OK, okConcept1.asJson)
    case (HttpMethods.PUT, uri)
        if uri.toString.endsWith(s"/datasets/1/concepts/model1") =>
      (StatusCodes.OK, okConcept1.asJson)
    case (HttpMethods.GET, uri)
        if uri.toString.endsWith(s"/datasets/1/concepts/model1/properties") =>
      (StatusCodes.OK, Seq(schemaProperty1).asJson)
    case (HttpMethods.GET, uri)
        if uri.toString.endsWith(s"/datasets/1/concepts/modelWithParent") =>
      (StatusCodes.OK, okConceptWithParent.asJson)
    case (HttpMethods.PUT, uri)
        if uri.toString.endsWith(s"/datasets/1/concepts/modelWithParent") =>
      (StatusCodes.OK, okConceptWithParent.asJson)
    case (HttpMethods.GET, uri)
        if uri.toString.endsWith(
          s"/datasets/1/concepts/modelWithParent/properties"
        ) =>
      (
        StatusCodes.OK,
        (Seq.empty: Seq[responses.modelservice.SchemaPropertyResponse]).asJson
      )
  }
}

class TemplatesHandlerSpec extends HelperSpec with ScalaFutures {

  val traceIdHeader = TraceIdHeader("1234-5678")

  override val responder =
    new TemplatesHandlerMockResponder()(system, executionContext, materializer)

  lazy val routes = Route.seal(TemplatesHandler.secureRoutes)

  val createSchemaRequest = CreateSchemaRequest(
    schema = "http://schema.pennsieve.net/model/draft-01/schema",
    name = "name1",
    description = "example template",
    properties = Map(
      "Required1" -> Map("type" -> "string", "description" -> "test property")
    ).asJson,
    required = List("Required1")
  )

  override def afterStart(): Unit = {
    super.afterStart

    val migrator = new MigrationRunner(
      container.jdbcUrl,
      container.username,
      container.password
    )
    val (count, _) = migrator.run()
    assert(count > 0)

    addRecord(OrganizationId(1), ModelId("template1"), "name1")
  }

  def addRecord(
    oId: OrganizationId,
    id: ModelId,
    name: String,
    icon: Option[Icon] = None
  ): TemplateSchemaRecord = {
    val record = TemplateSchemaRecord(
      id = id,
      organizationId = oId,
      schema = "http://schema.pennsieve.net/model/draft-01/schema",
      name = name,
      displayName = name.replaceAll(" ", "_").toLowerCase(),
      description = s"example template ${id.value}",
      properties = Map(
        "Required1" -> Map("type" -> "string", "description" -> "test property")
      ).asJson,
      required = List("Required1"),
      icon = icon
    )
    Await.result(msContainer.db.run(TemplateSchemaMapper += record), 5.seconds)

    record
  }

  "templateRoutes GET /organizations/:org_id" should {
    "return list of records when successful - simple" in {
      addRecord(OrganizationId(5), ModelId("template2"), "name1")
      addRecord(OrganizationId(5), ModelId("template3"), "name1")
      val r1 = addRecord(
        OrganizationId(5),
        ModelId("template4"),
        "name1",
        icon = Some(Icon.Experiment)
      )

      addRecord(
        OrganizationId(5),
        ModelId("template5"),
        "name2",
        icon = Some(Icon.Experiment)
      )
      val r2 = addRecord(
        OrganizationId(5),
        ModelId("template6"),
        "name2",
        icon = Some(Icon.Experiment)
      )

      val r3 = addRecord(
        OrganizationId(5),
        ModelId("template7"),
        "name3",
        icon = Some(Icon.Experiment)
      )

      val token = Token.generate(OrganizationId(5))(msContainer.jwt)
      val request = Get(uri = "/organizations/5/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        entityAs[List[TemplateSchema]] should ===(
          List(
            r1.toSchema(msContainer.prefixUrl),
            r2.toSchema(msContainer.prefixUrl),
            r3.toSchema(msContainer.prefixUrl)
          )
        )
      }
    }

    "return empty list when organization has no templates" in {
      val token = Token.generate(OrganizationId(10))(msContainer.jwt)
      val request = Get(uri = "/organizations/10/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)

        entityAs[List[Json]] should ===(List.empty)
      }
    }

    "return error when requesting for an unauthorized organization" in {
      val token = Token.generate(OrganizationId(2))(msContainer.jwt)
      val request = Get(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "return only gellery template models (no dataset template models)" in {
      val orgId = OrganizationId(1)
      val datasetId = Generators.randomDatasetId

      // A dedicated template gallery schema (model) should exist before this
      // test is started.

      // Add a dataset template:
      val dsTemplate: DatasetTemplate = Await
        .result(
          msContainer.db.run(
            DatasetTemplateMapper.insert(
              DatasetTemplate(
                organizationId = orgId,
                datasetId = datasetId,
                name = "DATASET-TEMPLATE-EXAMPLE",
                description = "DATASET-TEMPLATE-EXAMPLE"
              )
            )
          ),
          5.seconds
        )
      val randomDSModel =
        Generators.randomTemplateSchemaRecord(orgId, dsTemplate.id)
      Await.result(
        msContainer.db.run(TemplateSchemaMapper += randomDSModel),
        5.seconds
      )

      val token = Token.generate(orgId)(msContainer.jwt)
      val request = Get(uri = s"/organizations/${orgId.value}/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        val templates = entityAs[Seq[TemplateSchema]]
        templates should have size 1
        templates.head.id should not equal (randomDSModel.id.value)
        templates.head.name should not equal (randomDSModel.name)
        templates.head.description should not equal (randomDSModel.description)
      }
    }
  }

  "templateRoutes GET /organizations/:org_id/templates/:id" should {
    "return record when successful" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val request = Get(uri = "/organizations/1/templates/template1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)

        val response = Await
          .result(
            msContainer.db.run(
              TemplateSchemaMapper
                .getSchema(OrganizationId(1), ModelId("template1"))
            ),
            5.seconds
          )
          .toSchema(msContainer.prefixUrl)
        entityAs[TemplateSchema] should ===(response)
      }
    }

    "return not found when id is invalid" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val request = Get(uri = "/organizations/1/templates/template-invalid")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return not found when id is associated with a different organization" in {
      val token = Token.generate(OrganizationId(2))(msContainer.jwt)
      val request = Get(uri = "/organizations/2/templates/template1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return error when requesting for an unauthorized organization" in {
      val token = Token.generate(OrganizationId(2))(msContainer.jwt)
      val request = Get(uri = "/organizations/1/templates/template1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }
  }

  "templateRoutes DELETE /organizations/:org_id/templates/:id" should {
    "return not found when id is invalid" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val request = Delete(uri = "/organizations/1/templates/template-invalid")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return not found when id is associated with a different organization" in {
      val token = Token.generate(OrganizationId(2))(msContainer.jwt)
      val request = Delete(uri = "/organizations/2/templates/template1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return error when requesting for an unauthorized organization" in {
      val token = Token.generate(OrganizationId(2))(msContainer.jwt)
      val request = Delete(uri = "/organizations/1/templates/template1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "properly delete a schema record" in {
      val orgId = 6
      val schemaId = "delete-template-test"
      addRecord(OrganizationId(orgId), ModelId(schemaId), name = schemaId)
      val token = Token.generate(OrganizationId(orgId))(msContainer.jwt)

      // It exists:
      val getRequest =
        Get(uri = s"/organizations/${orgId}/templates/${schemaId}")
          .addHeader(Authorization(OAuth2BearerToken(token.value)))

      getRequest ~> routes ~> check {
        status should ===(StatusCodes.OK)
        val result = entityAs[TemplateSchema]
        result.name should ===("delete-template-test")
      }

      // Delete it:
      val deleteRequest =
        Delete(uri = s"/organizations/${orgId}/templates/${schemaId}")
          .addHeader(Authorization(OAuth2BearerToken(token.value)))

      deleteRequest ~> routes ~> check {
        status should ===(StatusCodes.OK)
        val result = entityAs[String]
        result should ===("\"Deleted template [delete-template-test]\"")
      }

      // Same request as earlier should fail:
      getRequest ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }
  }

  "templateRoutes POST /organizations/:org_id/templates" should {
    "create a new schema when successful" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(createSchemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val result =
          entityAs[TemplateSchemaRecord].toSchema(msContainer.prefixUrl)
        result.schema should ===(createSchemaRequest.schema)
      }
    }

    "create a new schema with a non-null category" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val json =
        """
          |{
          |  "$schema": "http://schema.pennsieve.net/model/draft-01/schema",
          |  "name": "name1",
          |  "description": "example template",
          |  "category": "cat1",
          |  "properties": {
          |    "Required1": {
          |      "type": "string",
          |      "description": "test property",
          |      "default": "Unknown"
          |    }
          |  },
          |  "required": ["Required1"]
          |}
        """.stripMargin

      val request = Post(
        uri = "/organizations/1/templates",
        HttpEntity(ContentTypes.`application/json`, json)
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val result =
          entityAs[TemplateSchemaRecord].toSchema(msContainer.prefixUrl)
        result.schema should ===(createSchemaRequest.schema)
        result.category should ===(Some("cat1"))
      }
    }

    "create a new schema with a valid icon" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val json =
        """
          |{
          |  "$schema": "http://schema.pennsieve.net/model/draft-01/schema",
          |  "name": "name1",
          |  "description": "example template",
          |  "properties": {
          |    "Required1": {
          |      "type": "string",
          |      "description": "test property",
          |      "default": "Unknown"
          |    }
          |  },
          |  "required": ["Required1"],
          |  "icon": "Experiment"
          |}
        """.stripMargin

      val request = Post(
        uri = "/organizations/1/templates",
        HttpEntity(ContentTypes.`application/json`, json)
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val result =
          entityAs[TemplateSchemaRecord].toSchema(msContainer.prefixUrl)
        result.schema should ===(createSchemaRequest.schema)
        result.icon should ===(Some(Icon.Experiment))
      }
    }

    "create new schema with an invalid icon silently set to null" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val json =
        """
          |{
          |  "$schema": "http://schema.pennsieve.net/model/draft-01/schema",
          |  "name": "name1",
          |  "description": "example template",
          |  "properties": {
          |    "Required1": {
          |      "type": "string",
          |      "description": "test property",
          |      "default": "Unknown"
          |    }
          |  },
          |  "required": ["Required1"],
          |  "icon": "InvalidIcon"
          |}
        """.stripMargin

      val request = Post(
        uri = "/organizations/1/templates",
        HttpEntity(ContentTypes.`application/json`, json)
      ).addHeader(Authorization(OAuth2BearerToken(token.value)))

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val result =
          entityAs[TemplateSchemaRecord].toSchema(msContainer.prefixUrl)
        result.schema should ===(createSchemaRequest.schema)
        result.icon should ===(None)
      }
    }

    "create a properties payload without an explicit string format" in {
      val schemaRequest = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = Map(
          "Required1" -> Map(
            "type" -> "string",
            "description" -> "test property"
          )
        ).asJson,
        required = List("Required1")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(schemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val found = payload.dataType
        val expected = (PropertyType.String: PropertyType).asJson
        found should ===(expected)
      }
    }

    "create a properties payload with an explicit string format" in {
      val schemaRequest = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = Map(
          "Required1" -> Map(
            "type" -> "string",
            "description" -> "test property",
            "format" -> "uri"
          )
        ).asJson,
        required = List("Required1")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(schemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val expected = payload.dataType
        val found = Json.obj(
          "type" -> (PropertyType.String: PropertyType).asJson,
          "format" -> "uri".asJson
        )
        found should ===(expected)
      }
    }

    "create a properties payload containing units for the concepts service" in {
      val unitsRequest = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = Map(
          "Required1" -> Map(
            "type" -> "number",
            "description" -> "test property",
            "unit" -> "inches"
          )
        ).asJson,
        required = List("Required1")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(unitsRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val found = payload.dataType
        val expected = Json.obj(
          "type" -> (PropertyType.Double: PropertyType).asJson,
          "unit" -> "inches".asJson
        )
        found should ===(expected)
      }
    }

    "create a concept properties payload containing a valid array" in {
      val propertiesJson =
        """{
           |  "Required1": {
           |    "type": "array",
           |    "description": "test property",
           |    "default": "bar",
           |    "items": {
           |      "type": "string",
           |      "enum": ["foo", "bar", "baz"]
           |    }
           |  }
           |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val req = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      ).asJson

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(req).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload = SchemaManager
          .toCreateModelPropertyRequestsForJSONTemplate(template)
          .toOption
          .get
          .head
        val found = payload.dataType
        val foundDefault = payload.defaultValue
        val expected = Json.obj(
          "type" -> "Array".asJson,
          "items" ->
            Json.obj(
              "type" -> (PropertyType.String: PropertyType).asJson,
              "enum" -> Set("foo", "bar", "baz").asJson
            )
        )
        found should ===(expected)
        foundDefault should ===(Some("bar".inject[InstanceValue]))
      }
    }

    "create a concept properties payload with enumerated string values" in {
      val propertiesJson =
        """{
          |  "Required1": {
          |    "type": "string",
          |    "description": "test property",
          |    "enum": ["foo", "bar", "baz"]
          |  }
          |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val req = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      ).asJson

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(req).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val found = payload.dataType
        val expected = Json.obj(
          "type" -> Json.fromString("Enum"),
          "items" ->
            Json.obj(
              "type" -> Json.fromString("String"),
              "enum" ->
                Json.fromValues(
                  Set(
                    Json.fromString("foo"),
                    Json.fromString("bar"),
                    Json.fromString("baz")
                  )
                )
            )
        )
        found should ===(expected)
      }
    }

    "create a concept properties payload with enumerated doubles (numbers) with a unit" in {
      val propertiesJson =
        """{
          |  "Required1": {
          |    "type": "number",
          |    "description": "test property",
          |    "unit": "inches",
          |    "enum": [1.0, 5.0, 10.0]
          |  }
          |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val req = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      ).asJson

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(req).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val found = payload.dataType
        val expected = Json.obj(
          "type" -> Json.fromString("Enum"),
          "items" ->
            Json.obj(
              "type" -> Json.fromString("Double"),
              "unit" -> Json.fromString("inches"),
              "enum" ->
                Json.fromValues(
                  Set(
                    Json.fromDouble(1.0).get,
                    Json.fromDouble(5.0).get,
                    Json.fromDouble(10.0).get
                  )
                )
            )
        )
        found should ===(expected)
      }
    }

    "create a properties payload with a valid default value - simple" in {

      val propertiesJson =
        """{
          |  "Required1": {
          |    "type": "string",
          |    "description": "test property",
          |    "default": "myDefault"
          |  }
          |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val schemaRequest = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(schemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val found = payload.defaultValue
        val expected = Some("myDefault".inject[InstanceValue])
        found should ===(expected)
      }
    }

    "create a properties payload with a valid default value - complex" in {

      val propertiesJson =
        """{
          |  "myProperty": {
          |    "type": "array",
          |    "description": "test property",
          |    "default": "bar",
          |    "items": {
          |      "type": "string",
          |      "enum": ["foo", "bar", "baz"]
          |    }
          |  }
          |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get

      val schemaRequest = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name2",
        description = "example template",
        properties = properties,
        required = List("myProperty")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(schemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val found = payload.defaultValue
        val expected = Some("bar".inject[InstanceValue])
        found should ===(expected)
      }
    }

    "create a properties payload with a valid default formatted string value" in {

      val propertiesJson =
        """{
          |  "Required1": {
          |    "type": "string",
          |    "description": "test property",
          |    "format": "email",
          |    "default": "test@test.com"
          |  }
          |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val schemaRequest = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(schemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val found = payload.defaultValue
        val expected = Some("test@test.com".inject[InstanceValue])
        found should ===(expected)
      }
    }

    "fail to create a properties payload with an incorrectly typed default value" in {

      val propertiesJson =
        """{
          |  "Required1": {
          |    "type": "number",
          |    "description": "test property",
          |    "default": "someString"
          |  }
          |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val schemaRequest = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(schemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val _ = entityAs[definitions.ValidationBadRequest]
      }
    }

    "fail to create a properties payload with an invalid default value - simple" in {

      val propertiesJson =
        """{
          |  "Required1": {
          |    "type": "string",
          |    "description": "test property",
          |    "enum": ["foo", "bar", "baz"],
          |    "default": "test"
          |  }
          |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val req = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      ).asJson

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(req).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val _ = entityAs[definitions.ValidationBadRequest]
      }
    }

    "fail to create a properties payload with an invalid default value - complex" in {

      val propertiesJson =
        """{
          |  "myProperty": {
          |    "type": "array",
          |    "description": "test property",
          |    "default": "test",
          |    "items": {
          |      "type": "string",
          |      "enum": ["foo", "bar", "baz"]
          |    }
          |  }
          |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get

      val schemaRequest = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name2",
        description = "example template",
        properties = properties,
        required = List("myProperty")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(schemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val _ = entityAs[definitions.ValidationBadRequest]
      }
    }

    "fail to create a properties payload with an invalid default formatted string value" in {

      val propertiesJson =
        """{
          |  "myProperty": {
          |    "type": "string",
          |    "description": "test property",
          |    "format": "email",
          |    "default": "invalidDefault"
          |  }
          |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get

      val schemaRequest = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name2",
        description = "example template",
        properties = properties,
        required = List("myProperty")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(schemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val _ = entityAs[definitions.ValidationBadRequest]
      }
    }

    "fail to create a concept properties payload when an array has an invalid element type" in {
      val propertiesJson =
        """{
           |  "Required1": {
           |    "type": "array",
           |    "description": "test property",
           |    "items": {
           |      "type": "foo",
           |      "enum": [1, 2, 3]
           |    }
           |  }
           |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val req = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      ).asJson

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(req).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val _ = entityAs[definitions.ValidationBadRequest]
      }
    }

    "fail to create a properties payload with array element type mismatch" in {
      val propertiesJson =
        """{
           |  "Required1": {
           |    "type": "array",
           |    "description": "test property",
           |    "items": {
           |      "type": "string",
           |      "enum": [1, 2, 3]
           |    }
           |  }
           |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val req = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      ).asJson

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(req).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val _ = entityAs[definitions.ValidationBadRequest]
      }
    }

    "create a bioportal branch from json" in {
      val propertiesJson =
        s"""{
           |  "Required1": {
           |    "type": "string",
           |    "description": "test bioportal property",
           |    "branches": [
           |      {
           |        "source": "BIOPORTAL",
           |        "acronym": "EXAMPLE",
           |        "uri": "$BIOPORTAL_TEST_HOST/example/request/uri",
           |        "name": "Example",
           |        "maxDepth": 1
           |      }
           |    ]
           |  }
           |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val req = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      ).asJson

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(req).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val found = payload.dataType
        val expected = Json.obj(
          ("type", "Enum".asJson),
          (
            "items",
            Json.obj(
              ("type", "String".asJson),
              ("enum", Set("child3", "child1", "child2", "grandchild1").asJson),
              (
                "branches",
                List(
                  Json.obj(
                    ("acronym", "EXAMPLE".asJson),
                    ("uri", s"$BIOPORTAL_TEST_HOST/example/request/uri".asJson),
                    ("name", "Example".asJson),
                    ("maxDepth", 1.asJson),
                    ("source", "BIOPORTAL".asJson)
                  )
                ).asJson
              )
            )
          )
        )
        found should ===(expected)
      }
    }

    "create an array of bioportal branches from json" in {
      val propertiesJson =
        s"""{
           |  "Required1": {
           |    "type": "array",
           |    "description": "test bioportal array property",
           |    "items": {
           |      "type": "string",
           |      "branches": [
           |        {
           |          "source": "BIOPORTAL",
           |          "acronym": "EXAMPLE",
           |          "uri": "$BIOPORTAL_TEST_HOST/example/request/uri",
           |          "name": "Example",
           |          "maxDepth": 1
           |        }
           |      ]
           |    }
           |  }
           |}
        """.stripMargin
      val properties: Json = parse(propertiesJson).toOption.get
      val req = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = properties,
        required = List("Required1")
      ).asJson

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(req).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]
        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head
        val found = payload.dataType

        val expected = Json.obj(
          "type" -> "Array".asJson,
          "items" ->
            Json.obj(
              "type" -> "String".asJson,
              "enum" -> Set("child3", "child1", "child2", "grandchild1").asJson,
              "branches" ->
                List(
                  Json.obj(
                    "acronym" -> "EXAMPLE".asJson,
                    "uri" -> s"$BIOPORTAL_TEST_HOST/example/request/uri".asJson,
                    "name" -> "Example".asJson,
                    "maxDepth" -> 1.asJson,
                    "source" -> "BIOPORTAL".asJson
                  )
                ).asJson
            )
        )
        found should ===(expected)
      }
    }

    "fail to create a schema with invalid json" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val template = """{"test": "field"}"""
      val entity = Marshal(template).to[MessageEntity].futureValue
      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        entityAs[String] should startWith("The request content was malformed")
      }
    }

    "fail to create a schema with invalid property names" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val testTemplate = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = Map(
          "GoodName" -> Map(
            "type" -> "string",
            "description" -> "property with valid name"
          ),
          "NameWith:Colon" -> Map(
            "type" -> "string",
            "description" -> "property with colon in name"
          ),
          "$badPrefix" -> Map(
            "type" -> "string",
            "description" -> "property with reserved prefix in name"
          ),
          "   " -> Map(
            "type" -> "string",
            "description" -> "property with whitespace in name"
          ),
          "" -> Map(
            "type" -> "string",
            "description" -> "property with empty name"
          )
        ).asJson,
        required = List.empty
      )

      val entity = Marshal(testTemplate).to[MessageEntity].futureValue
      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val result = entityAs[definitions.ValidationBadRequest]
        assert(result.message.contains("Invalid property names found"))
        assert(
          result.problems.get == Vector(
            "",
            "$badPrefix",
            "NameWith:Colon",
            "   "
          )
        )
      }
    }

    "fail to create a schema with an invalid url" in {
      val badRequest = CreateSchemaRequest(
        schema = "http://test.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = Map(
          "Required1" -> Map(
            "type" -> "string",
            "description" -> "test property"
          )
        ).asJson,
        required = List("Required1")
      )

      val token = Token.generate(OrganizationId(1))(msContainer.jwt)
      val entity = Marshal(badRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val found = entityAs[definitions.ValidationBadRequest]
        val expected =
          "Not a Pennsieve schema url: http://test.pennsieve.net/model/draft-01/schema"
        found.message should ===(expected)
      }
    }

    "fail to create a schema for an unauthorized organization" in {
      val wrongToken = Token.generate(OrganizationId(5))(msContainer.jwt)
      val entity = Marshal(createSchemaRequest).to[MessageEntity].futureValue

      val request = Post(uri = "/organizations/1/templates")
        .addHeader(Authorization(OAuth2BearerToken(wrongToken.value)))
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }
  }

  "templateRoutes POST /organizations/:org_id/templates/:id/datasets/:dataset_id" should {
    "be forbidden without org access" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(1)))(
          msContainer.jwt
        )

      val request = Post(uri = "/organizations/2/templates/1/datasets/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be forbidden without dataset access" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)

      val request = Post(uri = "/organizations/2/templates/1/datasets/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be forbidden with insufficient dataset access" in {
      val token = Token.generate(
        OrganizationId(1),
        datasetId = Some(DatasetId(1)),
        datasetRole = Role.Viewer
      )(msContainer.jwt)

      val request = Post(uri = "/organizations/2/templates/1/datasets/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be not found when invalid template" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(1)))(
          msContainer.jwt
        )

      val request = Post(uri = "/organizations/1/templates/1/datasets/1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "return 2xx when successful" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(1)))(
          msContainer.jwt
        )

      val request =
        Post(uri = "/organizations/1/templates/template1/datasets/1")
          .addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val _ = entityAs[Seq[definitions.SchemaPropertyResponse]]
      }
    }

    "return 4xx when concepts service rejects concept creation" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(2)))(
          msContainer.jwt
        )

      val request =
        Post(uri = "/organizations/1/templates/template1/datasets/2")
          .addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val found = entityAs[definitions.ModelServiceBadRequest]
        val expected = ModelServiceJsonError("bad format".asJson)
        found.message should ===(expected.message)
      }
    }

    "return 4xx when model properties are rejected" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(3)))(
          msContainer.jwt
        )

      val request =
        Post(uri = "/organizations/1/templates/template1/datasets/3")
          .addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val found = entityAs[definitions.ModelServiceBadRequest]
        val expected = InvalidModelProperties("too many titles".asJson)
        found.message should ===(expected.message)
      }
    }

    "return 4xx when authorization header is missing" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(1)))(
          msContainer.jwt
        )

      val request =
        Post(uri = "/organizations/1/templates/template1/datasets/1")

      request ~> routes ~> check {
        status should ===(StatusCodes.Unauthorized)
      }
    }
  }

  "templateRoutes POST /organizations/:org_id/datasets/:dataset_id/models/:model_id" should {
    "be forbidden without org access" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(1)))(
          msContainer.jwt
        )

      val request = Post(uri = "/organizations/2/datasets/1/models/model1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be forbidden without dataset access" in {
      val token = Token.generate(OrganizationId(1))(msContainer.jwt)

      val request = Post(uri = "/organizations/2/datasets/1/models/model1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be forbidden with insufficient dataset access" in {
      val token = Token.generate(
        OrganizationId(1),
        datasetId = Some(DatasetId(1)),
        datasetRole = Role.Viewer
      )(msContainer.jwt)

      val request = Post(uri = "/organizations/2/datasets/1/models/model1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Forbidden)
      }
    }

    "be not found when invalid model" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(1)))(
          msContainer.jwt
        )

      val request = Post(uri = "/organizations/1/datasets/1/models/model99")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.NotFound)
      }
    }

    "be successful when model and properties exist" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(1)))(
          msContainer.jwt
        )

      val request = Post(uri = "/organizations/1/datasets/1/models/model1")
        .addHeader(Authorization(OAuth2BearerToken(token.value)))
        .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[TemplateSchemaRecord]

        val payload =
          SchemaManager
            .toCreateModelPropertyRequestsForJSONTemplate(template)
            .toOption
            .get
            .head

        payload.dataType should ===("String".asJson)
      }
    }

    "establish a parent-child relationship" in {
      val token =
        Token.generate(OrganizationId(1), datasetId = Some(DatasetId(1)))(
          msContainer.jwt
        )

      val request =
        Post(uri = "/organizations/1/datasets/1/models/modelWithParent")
          .addHeader(Authorization(OAuth2BearerToken(token.value)))
          .addHeader(traceIdHeader)

      request ~> routes ~> check {
        status should ===(StatusCodes.Created)
        val template = entityAs[definitions.TemplateSchemaRecord]
        template.parentId should ===(Some("template1"))
      }
    }
  }
}
