package com.blackfynn.modelschema.managers

import java.net.URL
import java.text.SimpleDateFormat
import java.util.TimeZone

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpRequest
import akka.stream.ActorMaterializer
import cats.implicits._
import com.blackfynn.modelschema.clients.{ Bioportal, BranchContainer }
import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.db.TemplateSchemaMapper
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.model.implicits._
import com.blackfynn.modelschema.{ requests, _ }
import com.blackfynn.modelschema.util.HttpUtils
import com.blackfynn.service.utilities.HttpResponder
import io.circe.Json
import io.circe.syntax._
import org.apache.commons.validator.routines.EmailValidator
import org.everit.json.schema.loader.SchemaLoader
import org.json.{ JSONObject, JSONTokener }
import shapeless.Coproduct
import shapeless.ops.coproduct.Selector

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

object SchemaManager {

  /**
    * URI of the validation schema used to validate the structure of model descriptions
    */
  val validationSchemaURI = "http://schema.pennsieve.net/model/draft-01/schema"

  val unsupportedTypes =
    List(PropertyType.Object, PropertyType.Null)

  val utcTimeZone: TimeZone = TimeZone.getTimeZone("UTC")

  // Standard date & time format
  // ex: 2018-08-28T13:53:01.000-05:00
  val dateFormat = toFormat("yyyy-MM-dd")
  val timeFormat = toFormat("HH:mm:ss.SSSXXX")
  val dateTimeFormat = toFormat(
    s"${dateFormat.toPattern}'T'${timeFormat.toPattern}"
  )

  /**
    * Create a request object for creating a model in the model service based on a TemplateSchemaRecord instance.
    *
    * @param model
    * @return
    */
  def toCreateModel(
    model: TemplateSchemaRecord
  ): requests.modelservice.CreateOrUpdateModel = {
    requests.modelservice.CreateOrUpdateModel(
      templateId = model.id.value,
      name = model.name,
      displayName = model.displayName,
      description = model.description
    )
  }

  /**
    * Create a request object for creating model properties based on a TemplateSchemaRecord instance.
    *
    * Implementation note: the `properties` JSON field of the TemplateSchemaRecord instance will be decoded as
    * `Seq[responses.modelservice.SchemaPropertyResponse]`.
    *
    * @param template
    * @param system
    * @param executionContext
    * @param materializer
    * @param msContainer
    */
  def toCreateModelPropertyRequestsForModelService(
    template: TemplateSchemaRecord
  )(implicit
    system: ActorSystem,
    executionContext: ExecutionContext,
    materializer: ActorMaterializer,
    msContainer: Container
  ): Either[Throwable, Seq[requests.modelservice.CreateOrUpdateModelProperty]] =
    for {
      properties <- template.properties
        .as[Seq[responses.modelservice.SchemaPropertyResponse]]
    } yield
      properties.map { property =>
        property.toCreateRequest
      }

  /**
    * Create a request object for creating a relationship in the model service based on a TemplateSchemaRelationship
    * instance.
    *
    * @param relationship
    * @return
    */
  def toCreateRelationship(
    relationship: TemplateSchemaRelationship
  ): Either[Throwable, requests.modelservice.CreateRelationship] =
    Either.catchNonFatal(
      requests.modelservice.CreateRelationship(
        name = relationship.name,
        displayName = relationship.displayName,
        description = relationship.description,
        schema = relationship.schema,
        from = relationship.from.map { id: ModelId =>
          id.toUUID
        },
        to = relationship.to.map { id: ModelId =>
          id.toUUID
        }
      )
    )

  /**
    * Create a request object for creating a linked property in the model service based on a
    * TemplateSchemaLinkedProperty instance.
    *
    * @param linkedProperty
    * @return
    */
  def toCreateLinkedProperty(
    linkedProperty: TemplateSchemaLinkedProperty
  ): Either[Throwable, requests.modelservice.CreateLinkedProperty] =
    Either.catchNonFatal(
      requests.modelservice.CreateLinkedProperty(
        name = linkedProperty.name,
        displayName = linkedProperty.displayName,
        to = linkedProperty.to.toUUID,
        position = Some(linkedProperty.position)
      )
    )

  /**
    * Transform a model-service property type definition into a form the model service can consume.
    *
    * @param pt
    * @return
    */
  private def toModelDataType(prop: SchemaProperty): Either[Throwable, Json] = {
    def getSupportedEnumType(values: PropertyType.EnumerationValues) =
      prop.`type` match {
        case p @ PropertyType.Boolean => Right(p.asEnumOf(values).asJson)
        case p @ PropertyType.Double => Right(p.asEnumOf(values).asJson)
        case p @ PropertyType.Double(_) => Right(p.asEnumOf(values).asJson)
        case p @ PropertyType.Long => Right(p.asEnumOf(values).asJson)
        case p @ PropertyType.Long(_) => Right(p.asEnumOf(values).asJson)
        case p @ PropertyType.String => Right(p.asEnumOf(values).asJson)
        case p @ PropertyType.FormattedString(_) =>
          Right(p.asEnumOf(values).asJson)
        case p @ PropertyType.BranchString(_) =>
          Right(p.asEnumOf(values).asJson)
        case _ =>
          Left(TypeNotSupportedForEnumeration(prop.`type`.tag))
      }
    prop.enum match {
      case Some(values: PropertyType.EnumerationValues) => {
        for {
          enum <- getSupportedEnumType(values)
        } yield {
          val baseAttributes =
            Seq("type" -> prop.`type`.tag.asJson, "enum" -> enum)
          val attributes = baseAttributes ++ prop.`type`.attributes
          // Supported values that can appear in a model-service enumeration:
          // - boolean
          // - long
          // - double
          // - string
          // Transform: see if we should create an enumerated type to send to the model service:
          Json.obj(
            "type" -> "Enum".asJson,
            "items" ->
              Json.obj(attributes: _*)
          )
        }
      }
      case None => Right(prop.`type`.asJson) // just return the type as-is:
    }
  }

  private def toCreateModelPropertyRequest(
    template: TemplateSchemaRecord,
    name: String,
    prop: SchemaProperty,
    isTitle: Boolean
  ): Either[Throwable, requests.modelservice.CreateOrUpdateModelProperty] = {
    val convertedDataType: Either[Throwable, Json] = toModelDataType(prop)
    convertedDataType.map { dt: Json =>
      {
        requests.modelservice.CreateOrUpdateModelProperty(
          name = name.replaceAll(" ", "_").toLowerCase,
          displayName = name,
          description = prop.description.getOrElse(""),
          required = Some(template.required.contains(name)),
          dataType = dt,
          conceptTitle = isTitle,
          default = template.required.contains(name),
          defaultValue = prop.default
        )
      }
    }
  }

  /**
    * Creates requests for creating new properties in the model service from a TemplateSchemaRecord
    * instance using the logic used when accepting an uploaded JSON schema definition file.
    *
    * The .json field of the TemplateSchemaRecord (typed as Json) will be interpreted as a map
    * of the form
    *
    * {
    *   "${Prop1}" -> Models/SchemaProperty.asJson,
    *   "${Prop2}" -> Models/SchemaProperty.asJson,
    *   ...,
    *   "${PropN}" -> Models/SchemaProperty.asJson
    * }
    *
    * See SchemaManager#parseProperties() for details
    *
    * @param template
    * @param system
    * @param executionContext
    * @param materializer
    * @param msContainer
    * @return
    */
  def toCreateModelPropertyRequestsForJSONTemplate(
    template: TemplateSchemaRecord
  )(implicit
    system: ActorSystem,
    executionContext: ExecutionContext,
    materializer: ActorMaterializer,
    msContainer: Container
  ): Either[Throwable, List[
    requests.modelservice.CreateOrUpdateModelProperty
  ]] = {
    implicit val branchContainer = msContainer.branches

    val properties =
      for {
        parsedProperties <- parseProperties(template.properties)
        _ <- validateParsedProperties(parsedProperties)
        propertiesWithBranches <- populateBranchProperties(parsedProperties)
        collapsedProperties <- {
          for (property <- propertiesWithBranches) yield {
            val (name, prop) = property
            toCreateModelPropertyRequest(
              template,
              name,
              prop,
              property == propertiesWithBranches.head
            )
          }
        }.toList.sequence // collapse a List[Either[Throwable, ModelPropertiesResponse]] into Either[Throwable, List[ModelPropertiesResponse]]
      } yield collapsedProperties

    properties.map { props =>
      props.filterNot { p =>
        unsupportedTypes.contains(p.dataType)
      }
    }
  }

  /*
   * Parse schema properties out of a Json object
   */
  private def parseProperties(
    properties: Json
  ): Either[InvalidModelProperties, Map[String, SchemaProperty]] =
    properties
      .as[Map[String, SchemaProperty]]
      .leftMap(_ => InvalidModelProperties(properties))

  /*
   * Given the result from parseProperties, validate the parsed
   * properties
   */
  def validateParsedProperties(
    properties: Map[String, SchemaProperty]
  )(implicit
    system: ActorSystem,
    executionContext: ExecutionContext,
    materializer: ActorMaterializer,
    msContainer: Container
  ): Either[ErrorResponse, Map[String, SchemaProperty]] = {
    implicit val branchContainer = msContainer.branches
    for {
      _ <- validatePropertyNames(properties)
      _ <- validateBranchProperties(properties)
      _ <- validateDefaultValues(properties)
    } yield properties
  }

  /*
   * Given a properties Map, return a new properties Map with all
   * branch information filled in.
   */
  def populateBranchProperties(
    properties: Map[String, SchemaProperty]
  )(implicit
    executionContext: ExecutionContext,
    branchContainer: BranchContainer
  ): Either[ErrorResponse, Map[String, SchemaProperty]] = {

    def updateEnums[A](
      name: String,
      branches: List[PropertyType.Branch]
    )(
      updateInProperty: (PropertyType.EnumerationValues) => SchemaProperty
    ): Future[(String, SchemaProperty)] =
      Future
        .sequence(branches.map {
          case PropertyType.Branch(acronym, uri, _, maxDepth, _) =>
            branchContainer.bioportal
              .getFlattenedHierarchyFromClass(acronym, uri, maxDepth)
        })
        .map(
          (enums: List[List[String]]) =>
            (
              name,
              updateInProperty(
                Coproduct[PropertyType.EnumerationValues](enums.flatten.toSet)
              )
            )
        )

    val validations = Future.sequence(properties.map {
      case (name, value: SchemaProperty) =>
        value.`type` match {
          case prop @ PropertyType.Array(items: BranchArrayContents) =>
            updateEnums(name, items.branches) { (enum) =>
              value.copy(
                `type` = prop.copy(items = items.copy(enum = Some(enum)))
              )
            }
          case _ =>
            value.branches
              .map(updateEnums(name, _) { (enum) =>
                value.copy(enum = Some(enum))
              })
              .getOrElse(Future.successful((name, value)))
        }
    })
    Right(Await.result(validations, 30.seconds).toMap)
  }

  def validatePropertyNames(
    properties: Map[String, SchemaProperty]
  ): Either[InvalidPropertyNamesFound, Map[String, SchemaProperty]] = {
    val invalidNames = properties
      .filterKeys(
        name =>
          (name.matches("^\\s+$") || name.length == 0 || name.trim
            .startsWith("$") || name.contains(":"))
      )
      .keys
      .toList

    if (invalidNames.isEmpty) Right(properties)
    else Left(InvalidPropertyNamesFound(invalidNames))

  }

  def validateDefaultValues(
    properties: Map[String, SchemaProperty]
  ): Either[InvalidDefaultValuesFound, Map[String, SchemaProperty]] = {
    val invalidDefaults = properties
      .filterNot(
        property =>
          property._2.default match {
            case Some(d) => defaultIsValid(property._2, d)
            case None => true
          }
      )

    if (invalidDefaults.isEmpty) Right(properties)
    else Left(InvalidDefaultValuesFound(invalidDefaults.keySet.toList))
  }

  def defaultIsValid[T](
    property: SchemaProperty,
    value: InstanceValue
  )(implicit
    select: Selector[InstanceValue, T]
  ): Boolean = {

    property.`type` match {
      case PropertyType.Array(contents) =>
        contents match {
          case EnumeratedArrayContents(enumType, enumValues) =>
            enumType
              .asEnumOf(enumValues)
              .flatMap { allowedValues =>
                enumType.asTypeOf(value).map { convertedValue =>
                  allowedValues.contains(convertedValue)
                }
              }
              .getOrElse(false)
          case BranchArrayContents(_, _, _) =>
            false // we do not support a default value for branch-type properties at this time
        }
      case str @ PropertyType.FormattedString(format) =>
        str
          .asTypeOf(value)
          .map(givenString => validateFormat(givenString, format))
          .getOrElse(false)
      case _ =>
        property.enum match {
          case Some(v) =>
            property.`type`
              .asEnumOf(v)
              .flatMap { allowedValues =>
                property.`type`
                  .asTypeOf(value)
                  .map { convertedValue =>
                    allowedValues.contains(convertedValue)
                  }
              }
              .getOrElse(false)
          case None => property.`type`.asTypeOf(value).nonEmpty
        }
    }
  }

  def toFormat(syntax: String): SimpleDateFormat = {
    val df = new SimpleDateFormat(syntax)
    df.setTimeZone(utcTimeZone)
    df
  }

  def validateFormat(given: String, format: String): Boolean = {
    format.trim.toLowerCase match {
      case "date" => Try(dateFormat.parse(given)).isSuccess
      case "datetime" => Try(dateTimeFormat.parse(given)).isSuccess
      case "email" => EmailValidator.getInstance().isValid(given)
      case "time" => Try(timeFormat.parse(given)).isSuccess
      case "uri" => Try(new URL(given).toURI).isSuccess
      case _ => false
    }
  }

  /*
   * Validate branch properties:
   *
   * 1. Ping the provided bioportal URI to make sure it is valid
   * 2. Enforce restrictions on the depth
   */
  def validateBranchProperties(
    properties: Map[String, SchemaProperty]
  )(implicit
    executionContext: ExecutionContext,
    branchContainer: BranchContainer
  ): Either[InvalidBranchPropertiesFound, Map[String, SchemaProperty]] = {

    def validateBranches(
      name: String
    )(
      branches: List[PropertyType.Branch]
    ): List[Future[Either[(String, String), Unit]]] =
      branches.flatMap {
        case PropertyType.Branch(acronym, uri, _, maxDepth, _) =>
          List(
            branchContainer.bioportal
              .isClassIdValid(acronym, uri)
              .map(
                (isValid: Boolean) =>
                  if (isValid) Right(())
                  else
                    Left((name, s"Invalid bioportal URI: ${uri}"))
              )
          ) ++ List(
            Future.successful(
              if (maxDepth >= Bioportal.MAX_DEPTH)
                Left(
                  (
                    name,
                    s"Bioportal depth cannot exceed ${Bioportal.MAX_DEPTH}"
                  )
                )
              else Right(())
            )
          )
      }

    val validations = Future
      .sequence(properties.flatMap {
        case (name, value: SchemaProperty) =>
          value.`type` match {
            case PropertyType.Array(items: BranchArrayContents) =>
              validateBranches(name)(items.branches)
            case _ =>
              (value.branches map validateBranches(name))

              // this property does not have branches, no need to validate
                .getOrElse(List(Future.successful(Right(()))))
          }
      })
      .map { results =>
        val errors = results.filter(_.isLeft).map(_.left.get)
        if (errors.size > 0) Left(InvalidBranchPropertiesFound(errors.toList))
        else Right(properties)
      }

    Await.result(validations, 30.seconds)
  }

  def isValid(
    template: CreateSchemaRequest
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[SuccessResponse] = {

    if (template.schema.startsWith("http://schema.pennsieve.net/model/")) {
      val http: HttpResponder = msContainer.client.http

      parseProperties(template.properties)
        .flatMap(validateParsedProperties) match {
        case Left(e) => Future.failed(e)
        case Right(_) => {
          // validate json structure
          val request = http.responder(HttpRequest(uri = template.schema))

          HttpUtils.redirectOrResult(request)(http, ec).flatMap { response =>
            HttpUtils.getBody(response).flatMap { model =>
              val modelNode = new JSONObject(new JSONTokener(model))
              val asJson: String = template.asJson.noSpaces
              val templateSchemaNode = new JSONObject(new JSONTokener(asJson))
              Future
                .successful(
                  SchemaLoader.load(modelNode).validate(templateSchemaNode)
                )
                .map(_ => SuccessResponse("Validation was successful!"))
            }
          }
        }
      }
    } else {
      Future.failed(new InvalidSchemaUrl(template.schema))
    }
  }

  /**
    * Given an organization ID and a mode template body, create a new model described by the template.
    *
    * @param organizationId
    * @param icon
    * @param template
    * @param system
    * @param ec
    * @param mat
    * @param msContainer
    * @return
    */
  def create(
    organizationId: OrganizationId,
    template: CreateSchemaRequest,
    parentTemplateId: Option[ModelId] = None
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Future[TemplateSchemaRecord] = {
    isValid(template).flatMap { _ =>
      val record = TemplateSchemaRecord(
        id = ModelId(java.util.UUID.randomUUID().toString),
        organizationId = organizationId,
        schema = template.schema,
        name = template.name.replaceAll(" ", "_").toLowerCase(),
        displayName = template.name,
        description = template.description,
        category = template.category,
        properties = template.properties,
        required = template.required,
        icon = template.icon,
        parentId = parentTemplateId
      )

      // If the template is equal to its parent, there is no need to
      // create a new template
      val checkChanges = parentTemplateId match {
        case Some(parentId) => {
          msContainer.db
            .run(TemplateSchemaMapper.getSchema(organizationId, parentId))
            .map(
              parentTemplate =>
                if (parentTemplate.equalWithoutIds(record))
                  Failure(TemplateHasNotChangedException(parentId))
                else
                  Success(())
            )
        }
        case None => Future.unit
      }

      checkChanges.flatMap { _ =>
        val query = for {
          id <- TemplateSchemaMapper returning TemplateSchemaMapper.map(_.id) += record
          newSchema <- TemplateSchemaMapper.getSchema(organizationId, id)
        } yield newSchema

        msContainer.db.run(query.transactionally)
      }
    }
  }
}
