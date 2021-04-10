package com.blackfynn.modelschema

import com.blackfynn.models.Feature
import org.everit.json.schema.ValidationException
import com.blackfynn.modelschema.model._
import io.circe.Json

import scala.collection.JavaConverters._
import scala.concurrent.Future

sealed trait ModelSchemaException extends Throwable {
  val message: String
}

object ErrorResponse {
  def apply(message: String) = UnknownException(message)

  def from(error: Throwable): ErrorResponse = {
    error match {
      case e: ModelServiceJsonError => e
      case e: ValidationException =>
        SchemaValidationException(e.getAllMessages.asScala.toList)
      case e: ErrorResponse => e
      case e => UnknownException(e.getMessage)
    }
  }

  def recoverWithFn[T](
    errFn: PartialFunction[ErrorResponse, Future[T]]
  ): PartialFunction[Throwable, Future[T]] = {
    case e: Throwable =>
      errFn.applyOrElse(from(e), (_: ErrorResponse) => Future.failed(e))
  }
}

sealed trait ErrorResponse extends ModelSchemaException {
  val message: String
}

sealed trait ModelSchemaForbiddenException extends ErrorResponse
case class ModelSchemaForbiddenOrganizationException(organizationId: Int)
    extends ModelSchemaForbiddenException {
  override val message =
    s"User does not have access to organization $organizationId"
}
case class ModelSchemaForbiddenDatasetException(datasetId: Int)
    extends ModelSchemaForbiddenException {
  override val message = s"User does not have access to dataset $datasetId"
}

case class ModelSchemaMissingFeatureFlagException(
  organizationId: Int,
  featureFlag: Feature
) extends ModelSchemaForbiddenException {
  override val message =
    s"Organization $organizationId does not have the required feature flag: $featureFlag"
}

case object InvalidAuthorizationHeader extends ModelSchemaForbiddenException {
  override val message = "Invalid authorization header"
}

case class InvalidModelProperties(error: Json) extends ErrorResponse {
  override val message = s"Invalid model properties: ${error.spaces2}"
}

case class InvalidSchemaUrl(url: String) extends ErrorResponse {
  override val message = s"Not a Pennsieve schema url: ${url}"
}

case class InvalidSchemaId(id: ModelId) extends ErrorResponse {
  override val message = s"Not a Pennsieve schema id: ${id}"
}

case class InvalidDatasetTemplateId(id: DatasetTemplateId)
    extends ErrorResponse {
  override val message = s"Not a Pennsieve dataset template id: ${id}"
}

case class InvalidModelId(id: ModelId) extends ErrorResponse {
  override val message = s"Not a Pennsieve model id: ${id}"
}

case class InvalidTemplateSchemaFormat(schema: String) extends ErrorResponse {
  override val message = s"Not a valid template schema: ${schema}"
}

case class TemplateHasNotChangedException(templateId: ModelId)
    extends ErrorResponse {
  override val message = s"Template has not changed: ${templateId.value}"
}

case class InvalidPropertyNamesFound(val names: List[String])
    extends ErrorResponse {
  override val message =
    s"Invalid property names found: ${names.mkString("\n")}"
}

case class InvalidBranchPropertiesFound(val properties: List[(String, String)])
    extends ErrorResponse {
  override val message =
    "Invalid branch properties:\n" + properties
      .map(p => s"${p._1}: ${p._2}")
      .mkString("\n")
}

case class InvalidDefaultValuesFound(val names: List[String])
    extends ErrorResponse {
  override val message =
    s"Invalid default values found for properties: ${names.mkString("\n")}"
}

case class InvalidPropertyType(typeName: String) extends ErrorResponse {
  override val message = s"Invalid property type: ${typeName}"
}

case class UnsupportedArrayItemType(typeName: String) extends ErrorResponse {
  override val message = s"Unsupported array item type: ${typeName}"
}

case class TypeNotSupportedForEnumeration(typeName: String)
    extends ErrorResponse {
  override val message =
    s"Property type not supported for enumeration: ${typeName}"
}

case class MistypedArrayItems(typeName: String, enumValueString: String)
    extends ErrorResponse {
  override val message =
    s"Invalid items {${enumValueString}} for array of type ${typeName}"
}

case class ModelServiceJsonError(error: Json) extends ErrorResponse {
  override val message =
    s"Error communicating with the model service: ${error.spaces2}"
}

case class SchemaValidationException(val payload: List[String])
    extends ErrorResponse {
  override val message = "Errors found during schema validation:\n" + payload
    .mkString("\n")
}

case class DuplicateDatasetTemplateName(
  val datasetId: DatasetId,
  val name: String
) extends ErrorResponse {
  override val message =
    s"Duplicate template name for dataset ${datasetId.value}: ${name}"
}

case class UnknownException(message: String) extends ErrorResponse
