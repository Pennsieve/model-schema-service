package com.blackfynn.modelschema

import com.blackfynn.modelschema.model.PropertyType.EnumerationValues
import org.scalatest.{ Matchers, WordSpec }
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.model.SchemaProperty._
import com.blackfynn.modelschema.model.implicits._
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import shapeless.Coproduct
import shapeless.syntax.inject._

class MarshallingSpec extends WordSpec with Matchers {

  val exampleBranches = List(
    PropertyType
      .Branch(acronym = "EXAMPLE", uri = "uri", name = "Example", maxDepth = 1)
  )
  val exampleFormat = "uri"
  val exampleUnit = "mg"

  "PropertyTypes:Boolean" should {

    "marshall correctly" in {
      val p: PropertyType = PropertyType.Boolean
      p.asJson should ===(Json.fromString("Boolean"))
    }

    "unmarshall correctly" in {
      for (t <- List("BOOLEAN", "boolean", "Boolean")) {
        decode[PropertyType](s""""${t}"""") should ===(
          Right(PropertyType.Boolean)
        )
      }
    }
  }

  "PropertyTypes:String" should {
    "marshall correctly without a format or branches" in {
      val p: PropertyType = PropertyType.String
      p.asJson should ===(Json.fromString("String"))
    }

    "unmarshall correctly without a format or branches" in {
      decode[PropertyType]("\"String\"") should ===(Right(PropertyType.String))
      decode[PropertyType]("\"string\"") should ===(Right(PropertyType.String))
    }
  }

  "PropertyTypes:FormattedString" should {

    "marshall correctly with a format" in {
      val p: PropertyType = PropertyType.FormattedString(exampleFormat)
      p.asJson should ===(
        Json.obj(
          "type" -> Json.fromString("String"),
          "format" -> Json.fromString(exampleFormat)
        )
      )
    }

    "unmarshall correctly with a format" in {
      decode[PropertyType](
        s"""{ "type": "String", "format": "${exampleFormat}" }"""
      ) should ===(Right(PropertyType.FormattedString(exampleFormat)))
      decode[PropertyType](
        s"""{ "type": "string", "format": "${exampleFormat}" }"""
      ) should ===(Right(PropertyType.FormattedString(exampleFormat)))
    }
  }

  "PropertyTypes:BranchString" should {
    "marshall correctly with a branch" in {
      val p: PropertyType = PropertyType.BranchString(exampleBranches)
      p.asJson should ===(
        Json.obj(
          "type" -> Json.fromString("String"),
          "branches" -> exampleBranches.asJson
        )
      )
    }

    "unmarshall correctly with a branch" in {
      decode[PropertyType](
        s"""{ "type": "String", "branches": ${exampleBranches.asJson} }"""
      ) should ===(Right(PropertyType.BranchString(exampleBranches)))
      decode[PropertyType](
        s"""{ "type": "string", "branches": ${exampleBranches.asJson} }"""
      ) should ===(Right(PropertyType.BranchString(exampleBranches)))
    }
  }

  "PropertyTypes:Long" should {

    "marshall correctly without a unit" in {
      val p: PropertyType = PropertyType.Long
      p.asJson should ===(Json.fromString("Long"))
    }

    "marshall correctly with a unit" in {
      val p: PropertyType = PropertyType.Long(exampleUnit)
      p.asJson should ===(
        Json.obj(
          "type" -> Json.fromString("Long"),
          "unit" -> Json.fromString(exampleUnit)
        )
      )
    }

    "unmarshall correctly without a unit" in {
      decode[PropertyType]("\"Long\"") should ===(Right(PropertyType.Long))
      decode[PropertyType]("\"long\"") should ===(Right(PropertyType.Long))
    }

    "unmarshall correctly with a unit" in {
      decode[PropertyType](s"""{ "type": "Long", "unit": "${exampleUnit}" }""") should ===(
        Right(PropertyType.Long(exampleUnit))
      )
      decode[PropertyType](s"""{ "type": "long", "unit": "${exampleUnit}" }""") should ===(
        Right(PropertyType.Long(exampleUnit))
      )
    }
  }

  "PropertyTypes:Double" should {
    "marshall correctly without a unit" in {
      val p: PropertyType = PropertyType.Double
      p.asJson should ===(Json.fromString("Double"))
    }

    "marshall correctly with a unit" in {
      val p: PropertyType = PropertyType.Double(exampleUnit)
      p.asJson should ===(
        Json.obj(
          "type" -> Json.fromString("Double"),
          "unit" -> Json.fromString(exampleUnit)
        )
      )
    }

    "unmarshall correctly without a unit" in {
      for (t <- List("DOUBLE", "double", "Double")) {
        decode[PropertyType](s""""${t}"""") should ===(
          Right(PropertyType.Double)
        )
      }
    }

    "unmarshall correctly with a unit" in {
      decode[PropertyType](
        s"""{ "type": "Double", "unit": "${exampleUnit}" }"""
      ) should ===(Right(PropertyType.Double(exampleUnit)))
      decode[PropertyType](
        s"""{ "type": "double", "unit": "${exampleUnit}" }"""
      ) should ===(Right(PropertyType.Double(exampleUnit)))
    }
  }

  "PropertyTypes:Null" should {
    "marshall correctly" in {
      val p: PropertyType = PropertyType.Null
      p.asJson should ===(Json.fromString("Null"))
    }

    "unmarshall correctly" in {
      for (t <- List("NULL", "null", "Null")) {
        decode[PropertyType](s""""${t}"""") should ===(Right(PropertyType.Null))
      }
    }
  }

  "PropertyTypes:Object" should {
    "marshall correctly" in {
      val p: PropertyType = PropertyType.Object
      p.asJson should ===(Json.fromString("Object"))
    }

    "unmarshall correctly" in {
      for (t <- List("OBJECT", "object", "Object")) {
        decode[PropertyType](s""""${t}"""") should ===(
          Right(PropertyType.Object)
        )
      }
    }
  }

  "PropertyTypes:Array" should {

    val enumeratedStringArray: PropertyType = PropertyType.Array
      .build(PropertyType.String, Set("foo", "bar", "baz"))
      .toOption
      .get

    val enumeratedStringArrayJson = Json.obj(
      "type" -> Json.fromString("Array"),
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

    "marshall correctly for enumerations" in {
      enumeratedStringArray.asJson === (enumeratedStringArrayJson)
    }

    "unmarshall correctly for enumerations" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": {
          |    "type": "string",
          |    "enum": ["foo", "bar", "baz"]
          |  }
          |}
        """.stripMargin
      val decodedStringArray: PropertyType =
        decode[PropertyType](json).toOption.get
      enumeratedStringArray should ===(decodedStringArray)
    }

    "fail if item type is invalid" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": {
          |    "type": "not-valid",
          |    "enum": ["foo", "bar", "baz"]
          |  }
          |}
        """.stripMargin
      decode[PropertyType](json).isLeft should ===(true)
    }

    "fail if item type does not match enum values" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": {
          |    "type": "string",
          |    "enum": [1.0, 2.0, 3.0]
          |  }
          |}
        """.stripMargin
      decode[PropertyType](json).isLeft should ===(true)
    }

    val branchStringArray: PropertyType = PropertyType.Array(
      BranchArrayContents(PropertyType.String, exampleBranches)
    )

    val branchStringArrayJson = Json.obj(
      "type" -> Json.fromString("Array"),
      "items" ->
        Json.obj(
          "type" -> Json.fromString("String"),
          "branches" -> exampleBranches.asJson,
          "enum" -> Json.Null
        )
    )

    "marshall correctly for branches" in {
      branchStringArray.asJson should ===(branchStringArrayJson)
    }

    "unmarshall correctly for branches" in {
      val json =
        """
          |{
          |  "type": "array",
          |  "items": {
          |    "type": "string",
          |    "branches": [{
          |      "source": "BIOPORTAL",
          |      "acronym": "EXAMPLE",
          |      "uri": "uri",
          |      "name": "Example",
          |      "maxDepth": 1
          |    }]
          |  }
          |}
        """.stripMargin

      val decodedStringArray: PropertyType =
        decode[PropertyType](json).toOption.get
      branchStringArray should ===(decodedStringArray)
    }
  }

  "Model-Service SchemaProperty" should {

    val defaultSchemaProperty: SchemaProperty =
      SchemaProperty(`type` = PropertyType.String)

    val defaultJson = Json.obj("type" -> Json.fromString("string"))

    "marshall properly by default" in {
      defaultSchemaProperty.asJson should ===(defaultJson)
    }

    "unmarshall properly by default" in {
      val incomingJson =
        """
          |{
          |  "type": "string"
          |}
        """.stripMargin
      decode[SchemaProperty](incomingJson).isRight should ===(true)
    }

    "marshall defaultValue field properly" in {

      val testProperty = SchemaProperty(
        `type` = PropertyType.Long,
        default = Some(12345.inject[InstanceValue])
      )

      val jsonWithDefaultValue = Json.obj(
        ("type", Json.fromString("long")),
        ("default", Json.fromLong(12345))
      )

      testProperty.asJson should ===(jsonWithDefaultValue)

    }

    "unmarshall defaultValue field properly" in {

      val jsonWithDefaultValue = Json.obj(
        ("type", Json.fromString("long")),
        ("default", Json.fromLong(12345))
      )

      decode[SchemaProperty](jsonWithDefaultValue.noSpaces).isRight should ===(
        true
      )

    }

    "marshall enumerations properly" in {
      val prop = defaultSchemaProperty.copy(
        enum = Some(Coproduct[EnumerationValues](Set("foo", "bar", "baz")))
      )
      val json = Json.obj(
        "type" -> Json.fromString("string"),
        "enum" ->
          Json.fromValues(
            Set(
              Json.fromString("foo"),
              Json.fromString("bar"),
              Json.fromString("baz")
            )
          )
      )
      prop.asJson should ===(json)
    }

    "not lift unit into type if type is not appropriate when marshalled" in {
      val incomingJson =
        s"""
          |{
          |  "type": "string",
          |  "unit": "$exampleUnit"
          |
          |}
        """.stripMargin
      val prop: SchemaProperty =
        decode[SchemaProperty](incomingJson).toOption.get
      val expected =
        SchemaProperty(`type` = PropertyType.String, unit = Some(exampleUnit))
      prop should ===(expected)
    }

    "lift unit into type if type is appropriate when unmarshalled" in {
      val incomingJson =
        s"""
          |{
          |  "type": "double",
          |  "unit": "$exampleUnit"
          |
          |}
        """.stripMargin
      val prop: SchemaProperty =
        decode[SchemaProperty](incomingJson).toOption.get
      val expected = SchemaProperty(
        `type` = PropertyType.Double(exampleUnit),
        unit = Some(exampleUnit)
      )
      prop should ===(expected)
    }

    "not lift format into type if type is not appropriate when marshalled" in {
      val incomingJson =
        s"""
          |{
          |  "type": "double",
          |  "format": "$exampleFormat"
          |
          |}
        """.stripMargin
      val prop: SchemaProperty =
        decode[SchemaProperty](incomingJson).toOption.get
      val expected =
        SchemaProperty(
          `type` = PropertyType.Double,
          format = Some(exampleFormat)
        )
      prop should ===(expected)
    }

    "lift format into type if type is appropriate when unmarshalled" in {
      val incomingJson =
        s"""
          |{
          |  "type": "string",
          |  "format": "$exampleFormat"
          |
          |}
        """.stripMargin
      val prop: SchemaProperty =
        decode[SchemaProperty](incomingJson).toOption.get
      val expected = SchemaProperty(
        `type` = PropertyType.FormattedString(exampleFormat),
        format = Some(exampleFormat)
      )
      prop should ===(expected)
    }

    "not lift branches into type if type is not appropriate when marshalled" in {
      val incomingJson =
        s"""
          |{
          |  "type": "double",
          |  "branches": ${exampleBranches.asJson}
          |}
        """.stripMargin
      val prop: SchemaProperty =
        decode[SchemaProperty](incomingJson).toOption.get
      val expected =
        SchemaProperty(
          `type` = PropertyType.Double,
          branches = Some(exampleBranches)
        )
      prop should ===(expected)
    }

    "lift branches into type if type is appropriate when unmarshalled" in {
      val incomingJson =
        s"""
          |{
          |  "type": "string",
          |  "branches": ${exampleBranches.asJson}
          |}
        """.stripMargin
      val prop: SchemaProperty =
        decode[SchemaProperty](incomingJson).toOption.get
      val expected = SchemaProperty(
        `type` = PropertyType.BranchString(exampleBranches),
        branches = Some(exampleBranches)
      )
      prop should ===(expected)
    }
  }
}
