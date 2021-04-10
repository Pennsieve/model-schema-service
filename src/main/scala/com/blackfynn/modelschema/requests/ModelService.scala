package com.blackfynn.modelschema.requests.modelservice

import java.util.UUID

import com.blackfynn.modelschema.model.implicits._
import com.blackfynn.modelschema.model.{ PropertyType, SchemaProperty }
import com.blackfynn.modelschema.responses
import io.circe.{ DecodingFailure, Json }
import io.scalaland.chimney.dsl._

/**
  * Represents a request to create a new schema property in the model (formerly concept) service.
  *
  * @param id
  * @param name
  * @param displayName
  * @param dataType
  * @param locked
  * @param default
  * @param conceptTitle
  * @param description
  * @param required
  * @param defaultValue
  */
case class CreateOrUpdateModelProperty(
  name: String,
  displayName: String,
  description: String,
  required: Option[Boolean],
  dataType: Json,
  conceptTitle: Boolean = false,
  index: Int = 0,
  locked: Boolean = false,
  default: Boolean = false,
  defaultValue: Option[InstanceValue] = None,
  id: Option[UUID] = None
) {
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

/**
  * Represents a request to create a new model in the model (formerly concept) service.
  *
  * @param templateId
  * @param name
  * @param displayName
  * @param description
  * @param locked
  */
case class CreateOrUpdateModel(
  templateId: String,
  name: String,
  displayName: String,
  description: String,
  locked: Boolean = false
)

/**
  * Represents a request to create a new relationship in the model (formerly concept) service.
  *
  * @param name
  * @param displayName
  * @param description
  */
case class CreateRelationship(
  name: String,
  displayName: String,
  description: String,
  schema: Seq[responses.modelservice.SchemaPropertyResponse],
  from: Option[UUID],
  to: Option[UUID]
)

/**
  * Represents a request to create a new linked property in the model (formerly concept) service.
  *
  * @param name
  * @param displayName
  */
case class CreateLinkedProperty(
  name: String,
  displayName: String,
  to: UUID,
  position: Option[Long]
)
