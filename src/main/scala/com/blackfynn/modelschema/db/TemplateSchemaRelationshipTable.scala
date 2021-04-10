// Copyright (c) 2018 Blackfynn, Inc. All Rights Reserved.

package com.blackfynn.modelschema.db

import java.time.{ OffsetDateTime, ZoneOffset }

import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.responses
import com.blackfynn.modelschema.model.{
  DatasetTemplateId,
  ModelId,
  SchemaRelationshipId,
  TemplateSchemaRelationship
}

import scala.concurrent.ExecutionContext
import io.circe._
import io.circe.parser._
import slick.jdbc.{ GetResult, PositionedResult }

final class TemplateSchemaRelationshipTable(tag: Tag)
    extends Table[TemplateSchemaRelationship](
      tag,
      Some("public"),
      "template_schema_relationship"
    ) {
  def id = column[SchemaRelationshipId]("id", O.PrimaryKey, O.AutoInc)
  def name = column[String]("name")
  def displayName = column[String]("display_name")
  def description = column[String]("description")
  def schema =
    column[Seq[responses.modelservice.SchemaPropertyResponse]]("schema")
  def from = column[Option[ModelId]]("from_id")
  def to = column[Option[ModelId]]("to_id")
  def datasetTemplateId = column[DatasetTemplateId]("dataset_template_id")
  def createdAt = column[OffsetDateTime]("created_at")
  def deleted = column[Boolean]("deleted")

  def * =
    (
      name,
      displayName,
      description,
      schema,
      from,
      to,
      datasetTemplateId,
      createdAt,
      deleted,
      id
    ).mapTo[TemplateSchemaRelationship]
}

object TemplateSchemaRelationshipTable {

  implicit class PgPositionedResult(val r: PositionedResult) extends AnyVal {
    def nextSchemaId: ModelId = ModelId(r.nextString)
    def nextSchemaIdOption: Option[ModelId] =
      r.nextStringOption.map(ModelId(_))
    def nextDatasetTemplateId: DatasetTemplateId =
      DatasetTemplateId(r.nextInt)
    def nextSchemaRelationshipId: SchemaRelationshipId =
      SchemaRelationshipId(r.nextInt)
    def nextOffsetDateTime: OffsetDateTime =
      OffsetDateTime.ofInstant(r.nextTimestamp.toInstant, ZoneOffset.UTC)
    def nextJson: Json = {
      val json = r.nextString
      val parsed = parse(json)
      parsed.right.get
    }
    def nextSchemaProperties
      : Seq[responses.modelservice.SchemaPropertyResponse] = {
      nextJson.as[Seq[responses.modelservice.SchemaPropertyResponse]].right.get
    }
  }
}

object TemplateSchemaRelationshipMapper
    extends TableQuery(new TemplateSchemaRelationshipTable(_)) {

  import TemplateSchemaRelationshipTable._

  def notDeleted = this.filter(!_.deleted)

  def insert(
    record: TemplateSchemaRelationship
  ): DBIO[TemplateSchemaRelationship] =
    this returning this.map(_.id) into ((item, id) => item.copy(id = id)) += record

  implicit val getResult = GetResult(t => {
    val id = t.nextSchemaRelationshipId
    val name = t.nextString
    val displayName = t.nextString
    val description = t.nextString
    val schema = t.nextSchemaProperties
    val from = t.nextSchemaIdOption
    val to = t.nextSchemaIdOption
    val datasetTemplateId = t.nextDatasetTemplateId
    val createdAt = t.nextOffsetDateTime
    val deleted = t.nextBoolean
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
  })

  // Get all relationships for a dataset template
  def getRelationshipsForDatasetTemplate(
    datasetTemplateId: DatasetTemplateId
  )(implicit
    ec: ExecutionContext
  ): Query[TemplateSchemaRelationshipTable, TemplateSchemaRelationship, Seq] = {
    notDeleted
      .filter(r => r.datasetTemplateId === datasetTemplateId)
  }

  // Delete all relationships associated with a given dataset template id
  def deleteDatasetTemplateRelationships(
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
