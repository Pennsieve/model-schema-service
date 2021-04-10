package com.blackfynn.modelschema.responses.modelservice

import java.time.{ OffsetDateTime, ZoneOffset }
import java.util.UUID

import com.blackfynn.http.server.definitions
import io.circe._
import io.circe.shapes._
import io.circe.java8.time._
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import io.scalaland.chimney.dsl._
import com.blackfynn.modelschema.model.{ PropertyType, SchemaProperty }
import com.blackfynn.modelschema.requests
import com.blackfynn.modelschema.model.implicits._

/**
  * Types mapping to responses produced by the Pennsieve model (formerly concept) service.
  */
case class ModelResponse(
  id: UUID,
  name: String,
  displayName: String,
  description: Option[String],
  locked: Boolean,
  count: Int,
  propertyCount: Int,
  createdAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
  updatedAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
  templateId: Option[String] = None
) {
  def toCreateRequest(): requests.modelservice.CreateOrUpdateModel =
    templateId match {
      case None =>
        throw new IllegalArgumentException(
          "ModelResponse must have a templateId in order to create a ModelPayload."
        )
      case Some(tId) =>
        requests.modelservice.CreateOrUpdateModel(
          name = name,
          displayName = displayName,
          description = description.getOrElse(displayName),
          locked = locked,
          templateId = tId
        )
    }
}

object ModelResponse {
  implicit val encode = deriveEncoder[ModelResponse]
  implicit val decode = deriveDecoder[ModelResponse]
}

/**
  * Represents a response from creating a new  property in the model (formerly concept) service for a model
  * or relationship.
  * @param id
  * @param name
  * @param displayName
  * @param dataType
  * @param locked
  * @param default
  * @param conceptTitle
  * @param description
  * @param required
  * @param createdAt
  * @param updatedAt
  */
case class SchemaPropertyResponse(
  id: UUID,
  name: String,
  displayName: String,
  dataType: Json,
  index: Int,
  locked: Boolean,
  default: Boolean,
  conceptTitle: Boolean,
  description: String,
  required: Boolean,
  defaultValue: Option[Json] = None, // required by the API, but optional in the request, so it makes sense to make it optional here
  createdAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
  updatedAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC)
) {
  def toDTO: definitions.SchemaPropertyResponse =
    this
      .into[definitions.SchemaPropertyResponse]
      .withFieldComputed(_.id, _.id.toString)
      .transform

  def toCreateRequest(): requests.modelservice.CreateOrUpdateModelProperty =
    requests.modelservice.CreateOrUpdateModelProperty(
      name = this.name,
      displayName = this.displayName,
      dataType = this.dataType,
      index = this.index,
      locked = this.locked,
      default = this.default,
      conceptTitle = this.conceptTitle,
      description = this.description,
      required = if (this.required) {
        Some(true)
      } else {
        None
      },
      defaultValue = None // Some(this.defaultValue)
    )

  def toUpdateRequest(): requests.modelservice.CreateOrUpdateModelProperty =
    requests.modelservice.CreateOrUpdateModelProperty(
      id = Some(this.id),
      name = this.name,
      displayName = this.description,
      dataType = this.dataType,
      index = this.index,
      locked = this.locked,
      default = this.default,
      conceptTitle = this.conceptTitle,
      description = this.description,
      required = if (this.required) {
        Some(true)
      } else {
        None
      },
      defaultValue = None // Some(this.defaultValue)
    )

  def toSchemaProperty: Either[DecodingFailure, SchemaProperty] =
    dataType
      .as[PropertyType]
      .map(
        parsedType =>
          this
            .into[SchemaProperty]
            .withFieldComputed(_.`type`, _ => parsedType)
            .withFieldComputed(_.description, x => Some(x.description))
            .withFieldComputed(_.default, _ => None)
            .transform
      )
}

object SchemaPropertyResponse {
  implicit val encode = deriveEncoder[SchemaPropertyResponse]
  implicit val decode = deriveDecoder[SchemaPropertyResponse]
}

/**
  * A container that holds a model and corresponding properties
  * @param model
  * @param properties
  */
case class ModelAndProperties(
  model: ModelResponse,
  properties: Seq[SchemaPropertyResponse]
)

object ModelAndProperties {
  implicit val encode = deriveEncoder[ModelAndProperties]
  implicit val decode = deriveDecoder[ModelAndProperties]
}

case class RelationshipResponse(
  id: UUID,
  name: String,
  displayName: String,
  description: String,
  schema: Seq[SchemaPropertyResponse],
  from: Option[UUID],
  to: Option[UUID],
  createdAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
  updatedAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC)
) {
  def modelIds: Set[UUID] = (from.toList ++ to.toList).toSet
}

object RelationshipResponse {
  implicit val encode = deriveEncoder[RelationshipResponse]
  implicit val decode = deriveDecoder[RelationshipResponse]
}

case class LinkedPropertyResponse(
  id: UUID,
  name: String,
  displayName: String,
  position: Long,
  from: UUID,
  to: UUID,
  createdAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC),
  updatedAt: OffsetDateTime = OffsetDateTime.now(ZoneOffset.UTC)
) {
  def modelIds: Set[UUID] = Set(from, to)
}

object LinkedPropertyResponse {
  implicit val encode = deriveEncoder[LinkedPropertyResponse]
  implicit val decode = deriveDecoder[LinkedPropertyResponse]
}

case class LinkedPropertyTargetResponse(
  link: LinkedPropertyResponse,
  concept: UUID
)

object LinkedPropertyTargetResponse {
  implicit val encode = deriveEncoder[LinkedPropertyTargetResponse]
  implicit val decode = deriveDecoder[LinkedPropertyTargetResponse]
}

case class DatasetSchemaResponse(
  modelsAndProperties: Seq[ModelAndProperties],
  relationships: Seq[RelationshipResponse],
  linkedProperties: Seq[LinkedPropertyTargetResponse]
) {
  def modelIds: Set[UUID] =
    modelsAndProperties.map { case ModelAndProperties(model, _) => model.id }.toSet
}

object DatasetSchemaResponse {
  implicit val encode = deriveEncoder[DatasetSchemaResponse]
  implicit val decode = deriveDecoder[DatasetSchemaResponse]
}
