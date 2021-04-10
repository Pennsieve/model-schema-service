package com.blackfynn.modelschema

import java.lang.{ String => JavaString }
import java.time.{ OffsetDateTime, ZoneOffset }
import java.util.UUID

import cats.syntax.either._
import com.blackfynn.http.server.definitions
import enumeratum._
import io.circe._
import io.circe.generic.extras.semiauto._
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import io.circe.java8.time._
import io.circe.shapes._
import io.circe.syntax._
import io.scalaland.chimney.dsl._
import shapeless.ops.coproduct.{ Inject, Selector }
import shapeless.{ :+:, CNil, Coproduct }
import com.blackfynn.concepts.models.Icon

package model {
  import com.blackfynn.modelschema.managers.SchemaManager
  import com.blackfynn.modelschema.model.implicits.InstanceValue
  import com.blackfynn.modelschema.responses

  case class DatasetId(val value: Int) extends AnyVal

  case class OrganizationId(val value: Int) extends AnyVal

  case class ModelId(val value: JavaString) extends AnyVal {
    /// *** UNSAFE, can throw ***
    def toUUID: UUID = UUID.fromString(value)
  }

  case class DatasetTemplateId(val value: Int) extends AnyVal

  case class SchemaRelationshipId(val value: Int) extends AnyVal

  case class SchemaLinkedPropertyId(val value: Int) extends AnyVal

  case class SuccessResponse(message: JavaString)

  object util {

    /**
      * Given an error message, this method generates a circe decoding error that contains the JSON traversal history
      * where the error originally occurred.
      *
      * @param c
      * @param e
      * @return
      */
    def decodeError(c: ACursor, e: Throwable): DecodingFailure = {
      DecodingFailure.fromThrowable(e, c.history)
    }
  }

  /* --- ArrayContents ---------------------------------------------------------------------------------------------- */

  /**
    * A type to hold the contents of PropertyType.Array. `type` specifies the type of the items of the array.
    *
    * @param `type`
    */
  sealed trait ArrayContents {
    val `type`: PropertyType
  }

  object ArrayContents {

    /**
      * A Shapeless coproduct that encodes the valid types an array can contain.
      */
    type Items =
      Set[Boolean] :+: Set[Double] :+: Set[Long] :+: Set[JavaString] :+: CNil

    /**
      * Test if for the given `type`, the contents of `enum` can be interpreted as that type
      *
      * Example (1) => PropertyType.String + Set("foo", "bar", "baz") => true
      *
      * Example (2) => PropertyType.String + Set(1.0, 2.0, 3.0) => false
      *
      * @param select
      * @tparam T
      * @return
      */
    def isValid[T](
      `type`: PropertyType,
      enum: Items
    )(implicit
      select: Selector[Items, T]
    ): Boolean =
      `type`.asEnumOf(enum).nonEmpty

    /**
      * Given a primitive Scala type like String, Int, Long, Boolean, etc, inject the value into a Shapeless coproduct.
      *
      * @param c
      * @param inj
      * @tparam T
      * @return
      */
    private def arrayOf[T](
      c: ACursor
    )(implicit
      inj: Inject[Items, Set[T]],
      d: io.circe.Decoder[List[T]]
    ): Decoder.Result[Items] =
      c.as[List[T]].map(vs => Coproduct[Items](vs.toSet))

    implicit def decoder(implicit enc: Decoder[PropertyType]) =
      new Decoder[ArrayContents] {
        final def apply(c: HCursor): Decoder.Result[ArrayContents] =
          // if this cursor has an "enum" field, it will be treated as
          // EnumeratedArrayContents
          if (c.keys.get.toList.contains("enum"))
            for {
              `type` <- c.get[PropertyType]("type")
              enum <- `type` match {
                case PropertyType.Boolean =>
                  arrayOf[Boolean](c.downField("enum"))
                case PropertyType.Double =>
                  arrayOf[Double](c.downField("enum"))
                case PropertyType.Long =>
                  arrayOf[Long](c.downField("enum"))
                case PropertyType.String =>
                  arrayOf[JavaString](c.downField("enum"))
                case _ =>
                  Left(
                    util
                      .decodeError(c, UnsupportedArrayItemType(`type`.tag))
                  )
              }
            } yield EnumeratedArrayContents(`type` = `type`, enum = enum)
          else
            for {
              `type` <- c.downField("type").as[PropertyType]
              branches <- c.downField("branches").as[List[PropertyType.Branch]]
            } yield BranchArrayContents(`type`, branches)
      }

    implicit def encoder(implicit enc: Encoder[PropertyType]) =
      new Encoder[ArrayContents] {
        final def apply(arr: ArrayContents): Json = {
          arr match {
            case e: EnumeratedArrayContents =>
              Json.obj("type" -> e.`type`.asJson, "enum" -> e.enum.asJson)
            case b: BranchArrayContents =>
              Json.obj(
                "type" -> b.`type`.asJson,
                "branches" -> b.branches.asJson,
                "enum" -> b.enum.asJson
              )
          }
        }
      }
  }

  /**
    * A class to describe the contents of a branch array. Branches are
    * encoded as a list of branches, and the enum that comes as a
    * result of those branches are encoded in the same way that they
    * would be in an enumerated array.
    *
    * @param `type`
    * @param branches
    * @param enum
    */
  final case class BranchArrayContents(
    override val `type`: PropertyType,
    branches: List[PropertyType.Branch],
    enum: Option[ArrayContents.Items] = None
  ) extends ArrayContents

  /**
    * A class to describe the contents of an enumerated array. The
    * items themselves are encoded as a Shapeless coproduct of
    * `Set[T]` for varying types of `T`.
    *
    * @param `type`
    * @param enum
    */
  final case class EnumeratedArrayContents(
    override val `type`: PropertyType,
    enum: ArrayContents.Items
  ) extends ArrayContents

  /* --- PropertyType ----------------------------------------------------------------------------------------------- */

  sealed trait PropertyType extends EnumEntry {
    type T
    val tag: JavaString

    /**
      * Given a coproduct defining a collection of values `enum` and a type `T` associated with a case class of
      * `PropertyType`, this method test if `enum` can be selected as `Set[T]`.
      *
      * @param enum
      * @param selector
      * @return
      */
    protected def selectAsEnumOf(
      enum: PropertyType.EnumerationValues
    )(implicit
      selector: Selector[PropertyType.EnumerationValues, Set[T]]
    ): Option[Set[T]] = enum.select[Set[T]]

    def asEnumOf(enum: PropertyType.EnumerationValues): Option[Set[T]] = None

    protected def selectAsTypeOf(
      value: InstanceValue
    )(implicit
      selector: Selector[InstanceValue, T]
    ): Option[T] = value.select[T]

    def asTypeOf(value: InstanceValue): Option[T] = None

    // Used when this type is used for a collection like array or enum
    val attributes: Seq[(String, Json)] = Seq()
  }

  object PropertyType extends Enum[PropertyType] {

    val values = findValues

    type EnumerationValues = ArrayContents.Items

    // Normally, we should use types to encode *only* valid states of the application. An array without contents
    // does  not make sense (as it is effectively untyped), but it is needed by circe for intermediate decoding steps.
    // See `SchemaProperty.decoder` for the case in which this is needed.
    case object Array extends PropertyType {
      type T = Nothing
      val tag = "Array"

      override def asEnumOf(
        enum: PropertyType.EnumerationValues
      ): Option[Set[Nothing]] = None

      /**
        * Construct a new Array, validating the type `items` against the provided type `type`.
        *
        * @param `type`
        * @param enum
        * @param inj
        * @tparam T
        * @return
        */
      def build[T](
        `type`: PropertyType,
        enum: Set[T]
      )(implicit
        inj: Inject[EnumerationValues, Set[T]]
      ): Either[Throwable, PropertyType] =
        validate(`type`, Coproduct[EnumerationValues](enum))

      def validate(
        `type`: PropertyType,
        enum: EnumerationValues
      ): Either[Throwable, PropertyType] = {
        val contents = EnumeratedArrayContents(`type`, enum)
        if (ArrayContents.isValid(`type`, enum)) {
          Right(Array(contents))
        } else {
          Left(MistypedArrayItems(`type`.tag, enum.toString))
        }
      }
    }

    case class Array(items: ArrayContents) extends PropertyType {
      type T = Nothing
      val tag = "Array"
    }

    case object Boolean extends PropertyType {
      type T = scala.Boolean
      val tag = "Boolean"

      override def asTypeOf(value: InstanceValue) =
        selectAsTypeOf(value)
    }

    case object Double extends PropertyType {
      type T = scala.Double
      val tag = "Double"

      override def asEnumOf(enum: PropertyType.EnumerationValues) =
        selectAsEnumOf(enum)

      override def asTypeOf(value: InstanceValue) =
        selectAsTypeOf(value)
    }

    case class Double(unit: JavaString) extends PropertyType {
      type T = scala.Double
      val tag = "Double"

      override val attributes = Seq("unit" -> unit.asJson)

      override def asEnumOf(enum: PropertyType.EnumerationValues) =
        selectAsEnumOf(enum)

      override def asTypeOf(value: InstanceValue) =
        selectAsTypeOf(value)
    }

    case object Long extends PropertyType {
      type T = scala.Long
      val tag = "Long"

      override def asEnumOf(enum: PropertyType.EnumerationValues) =
        selectAsEnumOf(enum)

      override def asTypeOf(value: InstanceValue) =
        selectAsTypeOf(value)
    }

    case class Long(unit: JavaString) extends PropertyType {
      type T = scala.Long
      val tag = "Long"

      override val attributes = Seq("unit" -> unit.asJson)

      override def asEnumOf(enum: PropertyType.EnumerationValues) =
        selectAsEnumOf(enum)

      override def asTypeOf(value: InstanceValue) =
        selectAsTypeOf(value)
    }

    case object Null extends PropertyType {
      type T = Nothing
      val tag = "Null"
    }

    case object Object extends PropertyType {
      type T = Nothing
      val tag = "Object"
    }

    case object String extends PropertyType {
      type T = JavaString
      val tag = "String"

      override def asEnumOf(enum: PropertyType.EnumerationValues) =
        selectAsEnumOf(enum)

      override def asTypeOf(value: InstanceValue) =
        selectAsTypeOf(value)
    }

    case class FormattedString(format: JavaString) extends PropertyType {
      type T = JavaString
      val tag = "String"

      override val attributes = Seq("format" -> format.asJson)

      override def asEnumOf(enum: PropertyType.EnumerationValues) =
        selectAsEnumOf(enum)

      override def asTypeOf(value: InstanceValue) =
        selectAsTypeOf(value)
    }

    case class BranchString(branches: List[Branch]) extends PropertyType {
      type T = JavaString
      val tag = "String"

      override val attributes = Seq("branches" -> branches.map(_.asJson).asJson)

      override def asEnumOf(enum: PropertyType.EnumerationValues) =
        selectAsEnumOf(enum)

      override def asTypeOf(value: InstanceValue) =
        selectAsTypeOf(value)
    }

    case class Branch(
      val acronym: JavaString,
      val uri: JavaString,
      val name: JavaString,
      val maxDepth: Int,
      val source: JavaString = "BIOPORTAL"
    )

    case object Branch {
      implicit val encoder: Encoder[PropertyType.Branch] =
        deriveEncoder[PropertyType.Branch]
      implicit val decoder: Decoder[PropertyType.Branch] =
        deriveDecoder[PropertyType.Branch]
    }

    override def withNameInsensitiveOption(
      name: JavaString
    ): Option[PropertyType] = {
      name.toLowerCase match {
        case "number" => Some(Double)
        case "integer" | "int" => Some(Long)
        case _ => super.withNameInsensitiveOption(name)
      }
    }

    /**
      * Constructs the JSON associated with a "complex" `PropertyType`. All complex types are encoded as a JSON
      * object with a required string field "type" that specifies the type name and an arbitrary number of key-value
      * fields specifying properties unique to the type.
      *
      * Example (1):
      *
      * String => { "type": "String", "format": "uri" }
      *
      *
      * Example (2):
      *
      * Double => { "type": "Double", "unit": "inches" }
      *
      * Example (3):
      *
      * Array => { "type": "Array", "items": { "type": "String", "enum": ["foo", "bar", "baz"] } }
      *
      * @param `type`
      * @param attributes
      * @return
      */
    private def mkComplexType(
      `type`: PropertyType,
      attributes: (JavaString, Json)*
    ): Json =
      Json.obj(Seq("type" -> Json.fromString(`type`.tag)) ++ attributes: _*)

    /**
      * Like `mkComplexType`, but attempts to decode the resulting JSON into a `PropertyType`.
      *
      * @param `type`
      * @param attributes
      * @return
      */
    def asComplexType(
      `type`: PropertyType,
      attributes: (JavaString, Json)*
    ): Decoder.Result[PropertyType] =
      PropertyType.decoder.decodeJson(mkComplexType(`type`, attributes: _*))

    private def parseArrayType(c: HCursor): Decoder.Result[PropertyType] =
      for {
        contents <- c.get[ArrayContents]("items")
        array <- contents match {
          case EnumeratedArrayContents(contentsType, enum) =>
            Array.validate(contentsType, enum).left.map { t =>
              util.decodeError(c, t)
            }
          case b: BranchArrayContents => Right(Array(b))
        }
      } yield array

    private def parseStringType(c: HCursor): Decoder.Result[PropertyType] =
      parseFormattedStringType(c).recoverWith {
        case _ => parseBranchStringType(c)
      }

    private def parseFormattedStringType(
      c: HCursor
    ): Decoder.Result[PropertyType] =
      for {
        format <- c.get[JavaString]("format")
      } yield FormattedString(format)

    private def parseBranchStringType(
      c: HCursor
    ): Decoder.Result[PropertyType] =
      for {
        branches <- c.get[List[Branch]]("branches")
      } yield BranchString(branches)

    private def parseDoubleType(c: HCursor): Decoder.Result[PropertyType] =
      for {
        unit <- c.get[JavaString]("unit")
      } yield Double(unit)

    private def parseLongType(c: HCursor): Decoder.Result[PropertyType] =
      for {
        unit <- c.get[JavaString]("unit")
      } yield Long(unit)

    /**
      * Using a HCursor, try to parse the JSON area of focus as a "complex" type as described above.
      *
      * @param c
      * @return
      */
    def parseComplexType(c: HCursor): Decoder.Result[PropertyType] = {
      for {
        t <- c.get[JavaString]("type")
        value <- t.toLowerCase match {
          case "array" => parseArrayType(c)
          case "string" => parseStringType(c)
          case "double" | "number" => parseDoubleType(c)
          case "long" | "integer" | "int" => parseLongType(c)
          case _ =>
            Left(util.decodeError(c, InvalidPropertyType(t)))
        }
      } yield value
    }

    implicit val encoder = new Encoder[PropertyType] {
      final def apply(pt: PropertyType): Json = {
        pt match {
          case arr @ Array(contents) =>
            mkComplexType(arr, "items" -> ArrayContents.encoder(this)(contents))
          case FormattedString(format) =>
            mkComplexType(String, "format" -> format.asJson)
          case BranchString(branches) =>
            mkComplexType(String, "branches" -> branches.asJson)
          case Double(unit) =>
            mkComplexType(Double, "unit" -> unit.asJson)
          case Long(unit) =>
            mkComplexType(Long, "unit" -> unit.asJson)
          case _ => Json.fromString(pt.tag)
        }
      }
    }

    /**
      * A decoder for `PropertyType`
      *
      * Property types can be specified by a case-insensitive string, e.g. "Null", "Double", "date", "BOOLEAN", etc.
      *
      * Additionally, property types can be specified by a JSON object containing a discriminator field "type", which
      * encodes the property type name, as well as some number of fields unique to the type instance.
      *
      * Properties that use additional fields:
      *
      * - "String":
      *   - "format": Specify that the string expect to be formatted according to some scheme like "uri", "email",
      * or "date-time".
      *
      * - "Double"
      *   - "unit": Measurement unit
      *
      * - "Long"
      *   - "unit": Measurement unit
      */
    implicit val decoder = new Decoder[PropertyType] {
      final def apply(c: HCursor): Decoder.Result[PropertyType] = {
        c.as[JavaString] match {
          case Right(str) =>
            PropertyType
              .withNameInsensitiveOption(str)
              .toRight(util.decodeError(c, new Throwable(str)))
          case Left(_) => parseComplexType(c)
        }
      }
    }
  }

  // -------------------------------------------------------------------------------------------------------------------

  /**
    * Represents a type that holds information about a properties attached to data-modelling schema-level
    * objects like models (concepts) and relationships used by the model-schema-service (*NOT* MODEL SERVICE)
    *
    * @param `type`
    * @param description
    * @param unit
    * @param default
    * @param format
    * @param enum
    * @param branches
    */
  case class SchemaProperty(
    `type`: PropertyType,
    description: Option[JavaString] = None,
    unit: Option[JavaString] = None,
    default: Option[InstanceValue] = None,
    format: Option[JavaString] = None,
    enum: Option[PropertyType.EnumerationValues] = None,
    branches: Option[List[PropertyType.Branch]] = None
  )

  object SchemaProperty {
    implicit def encoder(
      implicit
      e: Encoder[PropertyType]
    ): Encoder[SchemaProperty] =
      Encoder
        .forProduct7(
          "type",
          "description",
          "unit",
          "default",
          "format",
          "enum",
          "branches"
        )(
          (t: SchemaProperty) =>
            (
              t.`type`,
              t.description,
              t.unit,
              t.default,
              t.format,
              t.enum,
              t.branches
            )
        )
        .mapJson { json =>
          // remove null values
          json.asObject.get.toVector
            .filter { case (_, value) => value != Json.Null }
            .map {
              // the json validator expects lowercase types
              case (k, v) =>
                if (k == "type")
                  (k, v.as[String].toOption.get.toLowerCase.asJson)
                else (k, v)
            }
            .toMap
            .asJson
        }

    private def defaultDecoder(
      implicit
      d: Decoder[PropertyType]
    ): Decoder[SchemaProperty] =
      Decoder.forProduct7(
        "type",
        "description",
        "unit",
        "default",
        "format",
        "enum",
        "branches"
      )(SchemaProperty.apply)

    implicit val decoder = new Decoder[SchemaProperty] {

      /**
        * A custom decoder that will handle lifting the appropriate properties from a `SchemaProperty` into its
        * contained `PropertyType`.
        *
        * Example [1]:
        *
        * "{ "type": "String", description: "foo", default: "foo", format: "uri" }
        *
        * becomes
        *
        * "{ "type": { "type": "String", "format": "uri" }, description: "foo", default: "foo", format: "uri" }
        *
        * Example [2]:
        *
        * "{ "type": "String", description: "foo", default: "foo" }
        *
        * remains
        *
        * "{ "type": "String", description: "foo", default: "foo" }
        *
        * @param c
        * @return
        */
      final def apply(c: HCursor): Decoder.Result[SchemaProperty] = {
        // Try to treat the cursor itself as a type, then inject the new type
        // (with contextual info, like format for strings, etc.) into the parsed
        // `SchemaProperty`
        val maybeType = PropertyType.parseComplexType(c)
        maybeType match {
          case Right(t) =>
            for {
              schemaProperty <- defaultDecoder.apply(c)
            } yield schemaProperty.copy(`type` = t)
          case Left(_) => defaultDecoder.apply(c)
        }
      }
    }
  }

  /**
    * Represents a user-submitted definition to create a model (concept) template.
    *
    * @param schema
    * @param name
    * @param description
    * @param category
    * @param properties
    * @param required
    * @param icon
    */
  case class CreateSchemaRequest(
    schema: JavaString,
    name: JavaString,
    description: JavaString,
    category: Option[JavaString] = None,
    properties: Json,
    required: List[JavaString],
    icon: Option[Icon] = None
  )

  /**
    * Represents a serializeable type that defines the metadata associated with a dataset template
    * (name, description, etc).
    *
    * @param id
    * @param organizationId
    * @param datasetId
    * @param name
    * @param description
    * @param createdAt
    * @param updatedAt
    * @param deleted
    */
  case class DatasetTemplate(
    organizationId: OrganizationId,
    datasetId: DatasetId,
    name: String,
    description: String,
    createdAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
    updatedAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
    deleted: Boolean = false,
    id: DatasetTemplateId = DatasetTemplateId(0)
  ) {
    def toDTO: definitions.DatasetTemplate =
      this
        .into[definitions.DatasetTemplate]
        .withFieldComputed(_.id, _.id.value)
        .withFieldComputed(_.organizationId, _.organizationId.value)
        .withFieldComputed(_.datasetId, _.datasetId.value)
        .withFieldComputed(_.createdAt, x => Some(x.createdAt))
        .withFieldComputed(_.updatedAt, x => Some(x.updatedAt))
        .withFieldComputed(_.deleted, x => Some(x.deleted))
        .transform
  }

  // slick does not support using classes with companion objects
  object DatasetTemplateObj {
    def fromDTO(dto: definitions.DatasetTemplate): DatasetTemplate =
      dto
        .into[DatasetTemplate]
        .withFieldComputed(_.id, dto => DatasetTemplateId(dto.id))
        .withFieldComputed(
          _.organizationId,
          dto => OrganizationId(dto.organizationId)
        )
        .withFieldComputed(_.datasetId, dto => DatasetId(dto.datasetId))
        .withFieldComputed(
          _.createdAt,
          _.createdAt.getOrElse(OffsetDateTime.now(ZoneOffset.UTC))
        )
        .withFieldComputed(
          _.updatedAt,
          _.updatedAt.getOrElse(OffsetDateTime.now(ZoneOffset.UTC))
        )
        .withFieldComputed(_.deleted, _.deleted.getOrElse(false))
        .transform
  }

  /**
    * Represents a serializeable collection representing a template of all scheme-level data-modelling objects belonging
    * to a dataset: models (concepts), properties, relationship, and linked properties.
    *
    * @param id
    * @param name
    * @param description
    * @param models
    * @param relationships
    * @param linkedProperties
    */
  case class DatasetTemplateSchema(
    id: DatasetTemplateId,
    name: JavaString,
    description: JavaString,
    models: Seq[TemplateSchemaRecord],
    relationships: Seq[TemplateSchemaRelationship],
    linkedProperties: Seq[TemplateSchemaLinkedProperty]
  ) {
    def modelIds: Set[ModelId] = models.map(_.id).toSet
    def toDTO: definitions.DatasetTemplateSchema =
      this
        .into[definitions.DatasetTemplateSchema]
        .withFieldComputed(_.id, _.id.value)
        .withFieldComputed(_.models, _.models.map(_.toDTO).toIndexedSeq)
        .withFieldComputed(
          _.relationships,
          _.relationships.map(_.toDTO).toIndexedSeq
        )
        .withFieldComputed(
          _.linkedProperties,
          _.linkedProperties.map(_.toDTO).toIndexedSeq
        )
        .transform
  }

  // TODO: Why do we have this? It seems like it could be totally replaced with the version generated by guardrail from
  //  the definition we have in model-schema-service.yaml
  /**
    * Represents a serializeable version of a schema-level model (concept).
    *
    * @param id
    * @param schema
    * @param name
    * @param displayName
    * @param description
    * @param category
    * @param properties
    * @param required
    * @param icon
    */
  case class TemplateSchema(
    id: JavaString,
    schema: JavaString,
    name: JavaString,
    displayName: JavaString,
    description: JavaString,
    category: Option[JavaString],
    properties: Json,
    required: List[JavaString],
    icon: Option[Icon]
  ) {
    def toDTO: definitions.TemplateSchema =
      this
        .into[definitions.TemplateSchema]
        .withFieldRenamed(_.id, _.$id)
        .withFieldRenamed(_.schema, _.$schema)
        .withFieldComputed(_.icon, _.icon.map(_.toString))
        .transform
  }

  /**
    * Represents a serializeable version of a schema-level model (concept).
    *
    * @param id
    * @param organizationId
    * @param schema
    * @param name
    * @param displayName
    * @param description
    * @param category
    * @param properties
    * @param required
    * @param createdAt
    * @param deleted
    * @param icon
    * @param parentId
    * @param datasetTemplateId
    */
  case class TemplateSchemaRecord(
    id: ModelId,
    organizationId: OrganizationId,
    schema: JavaString,
    name: JavaString,
    displayName: JavaString,
    description: JavaString,
    category: Option[JavaString] = None,
    properties: Json,
    required: List[JavaString],
    createdAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
    deleted: Boolean = false,
    icon: Option[Icon] = None,
    parentId: Option[ModelId] = None,
    datasetTemplateId: Option[DatasetTemplateId] = None
  ) {
    /*
     * See if two TemplateSchemaRecord objects are functionally
     * equivalent by comparing all fields except ID fields.
     */
    def equalWithoutIds(other: TemplateSchemaRecord): Boolean =
      organizationId == other.organizationId &&
        schema == other.schema &&
        name == other.name &&
        displayName == other.displayName &&
        description == other.description &&
        category == other.category &&
        properties == other.properties &&
        required == other.required &&
        createdAt == other.createdAt &&
        deleted == other.deleted &&
        icon == other.icon

    def toDTO: definitions.TemplateSchemaRecord =
      this
        .into[definitions.TemplateSchemaRecord]
        .withFieldComputed(_.$id, _.id.value)
        .withFieldComputed(_.$organizationId, _.organizationId.value)
        .withFieldComputed(_.icon, _.icon.map(_.toString))
        .withFieldComputed(_.parentId, _.parentId.map(_.value))
        .withFieldComputed(
          _.datasetTemplateId,
          _.datasetTemplateId.map(_.value)
        )
        .withFieldRenamed(_.schema, _.$schema)
        .transform

    def toSchema(prefix: JavaString): TemplateSchema =
      TemplateSchema(
        id =
          s"$prefix/organizations/${organizationId.value}/templates/${id.value}",
        schema = schema,
        name = name,
        displayName = displayName,
        description = description,
        category = category,
        properties = properties,
        required = required,
        icon = icon
      )
  }

  /**
    * Represents a serializeable version of a schema-level relationship.
    *
    * @param name
    * @param displayName
    * @param description
    * @param schema
    * @param from
    * @param to
    * @param datasetTemplateId
    * @param createdAt
    * @param deleted
    * @param id
    */
  case class TemplateSchemaRelationship(
    name: JavaString,
    displayName: JavaString,
    description: JavaString,
    schema: Seq[responses.modelservice.SchemaPropertyResponse],
    from: Option[ModelId],
    to: Option[ModelId],
    datasetTemplateId: DatasetTemplateId,
    createdAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
    deleted: Boolean = false,
    id: SchemaRelationshipId = SchemaRelationshipId(0)
  ) {

    /**
      * Return all model IDs this relationship points to.
      * @return
      */
    def modelIds: Set[ModelId] = (from.toList ++ to.toList).toSet

    def toDTO: definitions.TemplateSchemaRelationship =
      this
        .into[definitions.TemplateSchemaRelationship]
        .withFieldComputed(_.from, _.from.map(_.value))
        .withFieldComputed(_.to, _.to.map(_.value))
        .transform
  }

  /**
    * Represents a serializeable version of a schema-level linked-pr.
    *
    * @param name
    * @param displayName
    * @param position
    * @param from
    * @param to
    * @param datasetTemplateId
    * @param createdAt
    * @param deleted
    * @param id
    */
  case class TemplateSchemaLinkedProperty(
    name: JavaString,
    displayName: JavaString,
    position: Long,
    from: ModelId,
    to: ModelId,
    datasetTemplateId: DatasetTemplateId,
    createdAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
    deleted: Boolean = false,
    id: SchemaLinkedPropertyId = SchemaLinkedPropertyId(0)
  ) {

    /**
      * Return all model IDs this linked property points to.
      * @return
      */
    def modelIds: Set[ModelId] = Set(from, to)

    def toDTO: definitions.TemplateSchemaLinkedProperty =
      this
        .into[definitions.TemplateSchemaLinkedProperty]
        .withFieldComputed(_.from, _.from.value)
        .withFieldComputed(_.to, _.to.value)
        .transform
  }

  object implicits {

    // The InstanceValue type contains every acceptable type of property value
    type InstanceValue = String :+: Set[String] :+:
      Boolean :+: Set[Boolean] :+:
      Long :+: Set[Long] :+:
      Int :+: Set[Int] :+:
      Double :+: Set[Double] :+:
      CNil

    /*
     * Append methods to autogenerated guardrail classes to make it
     * easier to convert them to their pennsieve counterparts
     */
    implicit class GuardrailCreateSchemaRequest(
      req: definitions.CreateSchemaRequest
    ) {
      def toDomainObject: CreateSchemaRequest = {
        req
          .into[CreateSchemaRequest]
          .withFieldRenamed(_.$schema, _.schema)
          .withFieldComputed(
            _.icon,
            _.icon.flatMap(Icon.withNameInsensitiveOption(_))
          )
          .transform
      }
    }

    implicit class GuardrailTemplateSchema(schema: definitions.TemplateSchema) {
      def toDomainObject: TemplateSchema = {
        schema
          .into[TemplateSchema]
          .withFieldRenamed(_.$id, _.id)
          .withFieldRenamed(_.$schema, _.schema)
          .withFieldComputed(
            _.icon,
            _.icon.flatMap(Icon.withNameInsensitiveOption(_))
          )
          .transform
      }
    }

    implicit val propertyTypeDecoder = PropertyType.decoder
    implicit val propertyTypeEncoder = PropertyType.encoder

    implicit val datasetTemplateIdDecoder =
      deriveUnwrappedDecoder[DatasetTemplateId]
    implicit val datasetTemplateIdEncoder =
      deriveUnwrappedEncoder[DatasetTemplateId]

    implicit val schemaIdDecoder = deriveUnwrappedDecoder[ModelId]
    implicit val schemaIdEncoder = deriveUnwrappedEncoder[ModelId]

    implicit val schemaRelationshipIdDecoder =
      deriveUnwrappedDecoder[SchemaRelationshipId]
    implicit val schemaRelationshipIdEncoder =
      deriveUnwrappedEncoder[SchemaRelationshipId]

    implicit val schemaLinkedPropertyIdDecoder =
      deriveUnwrappedDecoder[SchemaLinkedPropertyId]
    implicit val schemaLinkedPropertyIdEncoder =
      deriveUnwrappedEncoder[SchemaLinkedPropertyId]

    implicit val datasetIdDecoder = deriveUnwrappedDecoder[OrganizationId]
    implicit val datasetIdEncoder = deriveUnwrappedEncoder[OrganizationId]

    implicit val organizationIdDecoder = deriveUnwrappedDecoder[DatasetId]
    implicit val organizationIdEncoder = deriveUnwrappedEncoder[DatasetId]

    implicit val successResponseEncoder = deriveEncoder[SuccessResponse]

    implicit val decodeDatasetTemplate: Decoder[DatasetTemplate] =
      Decoder.forProduct7(
        "id",
        "organizationId",
        "datasetId",
        "name",
        "description",
        "createdAt",
        "updatedAt"
      )(
        (
          id: DatasetTemplateId,
          organizationId: OrganizationId,
          datasetId: DatasetId,
          name: String,
          description: String,
          createdAt: OffsetDateTime,
          updatedAt: OffsetDateTime
        ) =>
          DatasetTemplate(
            organizationId = organizationId,
            datasetId = datasetId,
            name = name,
            description = description,
            createdAt = createdAt,
            updatedAt = updatedAt,
            id = id
          )
      )

    implicit val encodeDatasetTemplate: Encoder[DatasetTemplate] =
      Encoder.forProduct7(
        "id",
        "organizationId",
        "datasetId",
        "name",
        "description",
        "createdAt",
        "updatedAt"
      )(
        t =>
          (
            t.id,
            t.organizationId,
            t.datasetId,
            t.name,
            t.description,
            t.createdAt,
            t.updatedAt
          )
      )

    implicit val encodeDatasetTemplateSchema =
      deriveEncoder[DatasetTemplateSchema]
    implicit val decodeDatasetTemplateSchema =
      deriveDecoder[DatasetTemplateSchema]

    implicit val decodeCreateSchemaRequest: Decoder[CreateSchemaRequest] =
      Decoder.forProduct7(
        "$schema",
        "name",
        "description",
        "category",
        "properties",
        "required",
        "icon"
      )(
        (
          $schema: String,
          name: String,
          description: String,
          category: Option[String],
          properties: Json,
          required: List[String],
          icon: Option[Icon]
        ) =>
          CreateSchemaRequest(
            schema = $schema,
            name = name,
            description = description,
            category = category
              .flatMap(
                (cat: String) =>
                  if (cat.trim.nonEmpty) {
                    Some(cat)
                  } else {
                    None
                  }
              ),
            properties = properties,
            required = required,
            icon = icon
          )
      )

    implicit val encodeCreateSchemaRequest: Encoder[CreateSchemaRequest] =
      Encoder.forProduct6(
        "$schema",
        "name",
        "description",
        "category",
        "properties",
        "required"
      )(
        t =>
          (
            t.schema,
            t.name,
            t.description,
            t.category.getOrElse(""),
            t.properties,
            t.required
          )
      )

    implicit val decodeTemplateSchema: Decoder[TemplateSchema] =
      Decoder.forProduct9(
        "$id",
        "$schema",
        "name",
        "displayName",
        "description",
        "category",
        "properties",
        "required",
        "icon"
      )(TemplateSchema.apply)

    implicit val encodeTemplateSchema: Encoder[TemplateSchema] =
      Encoder.forProduct9(
        "$id",
        "$schema",
        "name",
        "displayName",
        "description",
        "category",
        "properties",
        "required",
        "icon"
      )(
        t =>
          (
            t.id,
            t.schema,
            t.name,
            t.displayName,
            t.description,
            t.category,
            t.properties,
            t.required,
            t.icon
          )
      )

    implicit val decodeTemplateSchemaRecord: Decoder[TemplateSchemaRecord] =
      Decoder.forProduct11(
        "$id",
        "$organizationId",
        "$schema",
        "name",
        "displayName",
        "description",
        "category",
        "properties",
        "required",
        "createdAt",
        "icon"
      )(
        (
          $id: ModelId,
          $organizationId: OrganizationId,
          $schema: String,
          name: String,
          displayName: String,
          description: String,
          category: Option[String],
          properties: Json,
          required: List[String],
          createdAt: OffsetDateTime,
          icon: Option[Icon]
        ) =>
          TemplateSchemaRecord(
            id = $id,
            organizationId = $organizationId,
            schema = $schema,
            name = name,
            displayName = displayName,
            description = description,
            category = category,
            properties = properties,
            required = required,
            createdAt = createdAt,
            icon = icon
          )
      )

    implicit val encodeTemplateSchemaRecord: Encoder[TemplateSchemaRecord] =
      Encoder.forProduct13(
        "$id",
        "$organizationId",
        "$schema",
        "name",
        "displayName",
        "description",
        "category",
        "properties",
        "required",
        "createdAt",
        "icon",
        "parentId",
        "datasetTemplateId"
      )(
        t =>
          (
            t.id,
            t.organizationId,
            t.schema,
            t.name,
            t.displayName,
            t.description,
            t.category,
            t.properties,
            t.required,
            t.createdAt,
            t.icon,
            t.parentId,
            t.datasetTemplateId
          )
      )

    implicit val decodeTemplateSchemaRelationship
      : Decoder[TemplateSchemaRelationship] =
      Decoder.forProduct10(
        "name",
        "displayName",
        "description",
        "schema",
        "from",
        "to",
        "datasetTemplateId",
        "createdAt",
        "deleted",
        "id"
      )(
        (
          name: String,
          displayName: String,
          description: String,
          schema: Seq[responses.modelservice.SchemaPropertyResponse],
          from: Option[ModelId],
          to: Option[ModelId],
          datasetTemplateId: DatasetTemplateId,
          createdAt: OffsetDateTime,
          deleted: Boolean,
          id: SchemaRelationshipId
        ) =>
          TemplateSchemaRelationship(
            id = id,
            name = name,
            displayName = displayName,
            description = description,
            schema = schema,
            from = from,
            to = to,
            datasetTemplateId = datasetTemplateId,
            createdAt = createdAt,
            deleted = deleted
          )
      )

    implicit val encodeTemplateSchemaRelationship
      : Encoder[TemplateSchemaRelationship] =
      Encoder.forProduct10(
        "name",
        "displayName",
        "description",
        "schema",
        "from",
        "to",
        "datasetTemplateId",
        "createdAt",
        "deleted",
        "id"
      )(
        t =>
          (
            t.name,
            t.displayName,
            t.description,
            t.schema,
            t.from,
            t.to,
            t.datasetTemplateId,
            t.createdAt,
            t.deleted,
            t.id
          )
      )

    implicit val decodeTemplateSchemaLinkedProperty
      : Decoder[TemplateSchemaLinkedProperty] =
      Decoder.forProduct9(
        "name",
        "displayName",
        "position",
        "from",
        "to",
        "datasetTemplateId",
        "createdAt",
        "deleted",
        "id"
      )(
        (
          name: String,
          displayName: String,
          position: Long,
          from: ModelId,
          to: ModelId,
          datasetTemplateId: DatasetTemplateId,
          createdAt: OffsetDateTime,
          deleted: Boolean,
          id: SchemaLinkedPropertyId
        ) =>
          TemplateSchemaLinkedProperty(
            id = id,
            name = name,
            displayName = displayName,
            position = position,
            from = from,
            to = to,
            datasetTemplateId = datasetTemplateId,
            createdAt = createdAt,
            deleted = deleted
          )
      )

    implicit val encodeTemplateSchemaLinkedProperty
      : Encoder[TemplateSchemaLinkedProperty] =
      Encoder.forProduct9(
        "name",
        "displayName",
        "position",
        "from",
        "to",
        "datasetTemplateId",
        "createdAt",
        "deleted",
        "id"
      )(
        t =>
          (
            t.name,
            t.displayName,
            t.position,
            t.from,
            t.to,
            t.datasetTemplateId,
            t.createdAt,
            t.deleted,
            t.id
          )
      )
  }
}
