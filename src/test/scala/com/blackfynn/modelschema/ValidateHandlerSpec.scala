package com.blackfynn.modelschema

import akka.http.scaladsl.marshalling.Marshal
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.Unmarshaller._
import com.blackfynn.http.server.definitions
import com.blackfynn.modelschema.model.CreateSchemaRequest
import com.blackfynn.modelschema.model.implicits._
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.syntax._
import io.circe.generic.auto._

import org.scalatest.concurrent.ScalaFutures

class ValidateHandlerSpec extends HelperSpec with ScalaFutures {

  override val responder =
    new TemplatesHandlerMockResponder()(system, executionContext, materializer)

  lazy val routes = Route.seal(ValidateHandler.routes)

  "validateRoutes POST /validate" should {
    "return 201 when successful" in {
      val template = CreateSchemaRequest(
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
      val entity = Marshal(template).to[MessageEntity].futureValue
      val request = Post(uri = "/validate")
        .withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
      }
    }

    "return schema information when invalid" in {
      val template = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = Map(
          "Required1" -> Map(
            "type" -> "string",
            "test" -> "badValue",
            "description" -> "test property"
          ),
          "Required2" -> Map(
            "type" -> "string",
            "description" -> "test property 2"
          )
        ).asJson,
        required = List("Required1")
      )
      val entity = Marshal(template).to[MessageEntity].futureValue
      val request = Post(uri = "/validate").withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)

        val found = entityAs[definitions.ValidationBadRequest]
        assert(found.message.contains("Errors found during schema validation"))
        assert(
          found.problems.get == Vector(
            "#/properties/Required1: extraneous key [test] is not permitted"
          )
        )

      }
    }

    "return bad request when non Pennsieve schema" in {
      val template = CreateSchemaRequest(
        schema = "http://bad.schema.pennsieve.net/model/draft-01/schema",
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
      val entity = Marshal(template).to[MessageEntity].futureValue
      val request = Post(uri = "/validate").withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val found = entityAs[definitions.ValidationBadRequest]
        val expected =
          "Not a Pennsieve schema url: http://bad.schema.pennsieve.net/model/draft-01/schema"
        found.message should ===(expected)
      }
    }

    "return bad request when template isn't formatted correctly" in {
      val template = """{"garbage": "field"}"""
      val entity = Marshal(template).to[MessageEntity].futureValue
      val request = Post(uri = "/validate").withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        entityAs[String] should startWith("The request content was malformed")
      }
    }

    "return bad request when template properties cannot be parsed" in {
      val template = CreateSchemaRequest(
        schema = "http://schema.pennsieve.net/model/draft-01/schema",
        name = "name1",
        description = "example template",
        properties = Map(
          "Required1" -> Map("type" -> "foo", "description" -> "test property"),
          "Required2" -> Map(
            "type" -> "incorrect",
            "description" -> "test property"
          )
        ).asJson,
        required = List("Required1")
      )
      val entity = Marshal(template).to[MessageEntity].futureValue
      val request = Post(uri = "/validate").withEntity(entity)

      request ~> routes ~> check {
        status should ===(StatusCodes.BadRequest)
        val found = entityAs[definitions.ValidationBadRequest]
        assert(found.message.contains("Invalid model properties"))

      }
    }

    "return bad request when template contains invalid property names" in {
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
      val request = Post(uri = "/validate")
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

  }

}
