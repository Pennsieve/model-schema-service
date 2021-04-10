// Copyright (c) 2018 Blackfynn, Inc. All Rights Reserved.

package com.blackfynn.modelschema.db

import java.time.{ OffsetDateTime, ZoneOffset }

import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.model.{
  DatasetTemplateId,
  ModelId,
  SchemaLinkedPropertyId,
  TemplateSchemaLinkedProperty
}

import scala.concurrent.ExecutionContext
import slick.jdbc.{ GetResult, PositionedResult }

final class TemplateSchemaLinkedPropertyTable(tag: Tag)
    extends Table[TemplateSchemaLinkedProperty](
      tag,
      Some("public"),
      "template_schema_linked_property"
    ) {
  def id = column[SchemaLinkedPropertyId]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def displayName = column[String]("display_name")
  def position = column[Long]("position")
  def from = column[ModelId]("from_id")
  def to = column[ModelId]("to_id")
  def datasetTemplateId = column[DatasetTemplateId]("dataset_template_id")
  def createdAt = column[OffsetDateTime]("created_at")
  def deleted = column[Boolean]("deleted")

  def * =
    (
      name,
      displayName,
      position,
      from,
      to,
      datasetTemplateId,
      createdAt,
      deleted,
      id
    ).mapTo[TemplateSchemaLinkedProperty]
}

object TemplateSchemaLinkedPropertyTable {

  implicit class PgPositionedResult(val r: PositionedResult) extends AnyVal {
    def nextSchemaId: ModelId = ModelId(r.nextString)
    def nextDatasetTemplateId: DatasetTemplateId =
      DatasetTemplateId(r.nextInt)
    def nextSchemaLinkedPropertyId: SchemaLinkedPropertyId =
      SchemaLinkedPropertyId(r.nextInt)
    def nextOffsetDateTime: OffsetDateTime =
      OffsetDateTime.ofInstant(r.nextTimestamp.toInstant, ZoneOffset.UTC)
  }
}

object TemplateSchemaLinkedPropertyMapper
    extends TableQuery(new TemplateSchemaLinkedPropertyTable(_)) {

  import TemplateSchemaLinkedPropertyTable._

  def notDeleted = this.filter(!_.deleted)

  def insert(
    record: TemplateSchemaLinkedProperty
  ): DBIO[TemplateSchemaLinkedProperty] =
    this returning this.map(_.id) into ((item, id) => item.copy(id = id)) += record

  implicit val getResult = GetResult(t => {
    val id = t.nextSchemaLinkedPropertyId
    val name = t.nextString
    val displayName = t.nextString
    val position = t.nextLong
    val from = t.nextSchemaId
    val to = t.nextSchemaId
    val datasetTemplateId = t.nextDatasetTemplateId
    val createdAt = t.nextOffsetDateTime
    val deleted = t.nextBoolean
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
  })

  // Get all linked properties for a dataset template
  def getLinkedPropertiesForDatasetTemplate(
    datasetTemplateId: DatasetTemplateId
  )(implicit
    ec: ExecutionContext
  ): Query[
    TemplateSchemaLinkedPropertyTable,
    TemplateSchemaLinkedProperty,
    Seq
  ] = {
    notDeleted
      .filter(r => r.datasetTemplateId === datasetTemplateId)
  }

  // Delete all linked properties associated with a given dataset template id
  def deleteDatasetTemplateLinkedProperties(
    datasetTemplateId: DatasetTemplateId
  )(implicit
    ec: ExecutionContext
  ): DBIOAction[Int, NoStream, Effect.Write] = {
    for {
      updateCount <- notDeleted
        .filter(_.datasetTemplateId === datasetTemplateId)
        .map(_.deleted)
        .update(true)
    } yield updateCount
  }
}
