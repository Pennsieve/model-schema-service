// Copyright (c) 2017 Blackfynn, Inc. All Rights Reserved.

package com.blackfynn.modelschema.db

import com.blackfynn.concepts.models.Icon
import com.blackfynn.modelschema.model.{
  DatasetTemplateId,
  ModelId,
  OrganizationId,
  TemplateSchemaRecord
}
import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.InvalidSchemaId
import io.circe._
import io.circe.parser._
import slick.jdbc.{
  GetResult,
  PositionedParameters,
  PositionedResult,
  SetParameter
}

import scala.concurrent.ExecutionContext
import java.time.{ OffsetDateTime, ZoneOffset }

final class TemplateSchemaTable(tag: Tag)
    extends Table[TemplateSchemaRecord](tag, Some("public"), "template_schema") {

  // TODO add index to org_id and check any other potential columns
  def id = column[ModelId]("id", O.PrimaryKey)
  def organizationId = column[OrganizationId]("org_id")
  def schema = column[String]("schema")
  def name = column[String]("name")
  def displayName = column[String]("display_name")
  def description = column[String]("description")
  def category = column[Option[String]]("category")
  def properties = column[Json]("properties")
  def required = column[List[String]]("required")
  def createdAt = column[OffsetDateTime]("created_at")
  def deleted = column[Boolean]("deleted")
  def icon = column[Option[Icon]]("icon")
  def parentId = column[Option[ModelId]]("parent_id")
  def datasetTemplateId =
    column[Option[DatasetTemplateId]]("dataset_template_id")

  def * =
    (
      id,
      organizationId,
      schema,
      name,
      displayName,
      description,
      category,
      properties,
      required,
      createdAt,
      deleted,
      icon,
      parentId,
      datasetTemplateId
    ).mapTo[TemplateSchemaRecord]
}

object TemplateSchemaTable {

  implicit class PgPositionedResult(val r: PositionedResult) extends AnyVal {
    def nextSchemaId: ModelId = ModelId(r.nextString)
    def nextSchemaIdOption: Option[ModelId] =
      r.nextStringOption().map(ModelId)
    def nextDatasetTemplateIdOption: Option[DatasetTemplateId] =
      r.nextIntOption().map(DatasetTemplateId)
    def nextOrganizationId: OrganizationId = OrganizationId(r.nextInt)
    def nextOffsetDateTime: OffsetDateTime =
      OffsetDateTime.ofInstant(r.nextTimestamp.toInstant, ZoneOffset.UTC)
    def nextJson: Json = {
      val json = r.nextString
      val parsed = parse(json)
      parsed.right.get
    }
    def nextListString: List[String] = {
      val json = r.nextString
      val values = decode[List[String]](json)
      values.right.get
    }
    def nextIcon: Option[Icon] = {
      r.nextStringOption.flatMap(Icon.withNameInsensitiveOption(_))
    }
  }

  implicit object SetOrganizationId extends SetParameter[OrganizationId] {
    def apply(oId: OrganizationId, pp: PositionedParameters) =
      pp.setInt(oId.value)
  }
}

object TemplateSchemaMapper extends TableQuery(new TemplateSchemaTable(_)) {

  import TemplateSchemaTable._

  def insert(record: TemplateSchemaRecord): DBIO[TemplateSchemaRecord] =
    this returning this.map(_.id) into ((item, id) => item.copy(id = id)) += record

  def notDeleted: Query[TemplateSchemaTable, TemplateSchemaRecord, Seq] =
    this.filter(!_.deleted)

  def get(oId: OrganizationId, id: ModelId) =
    this.filter(r => r.id === id && r.organizationId === oId && !r.deleted)

  def getSchema(
    oId: OrganizationId,
    id: ModelId
  )(implicit
    ec: ExecutionContext
  ): DBIO[TemplateSchemaRecord] = {
    this
      .get(oId, id)
      .result
      .headOption
      .flatMap {
        case None => {
          DBIO.failed(InvalidSchemaId(id))
        }
        case Some(s: TemplateSchemaRecord) => {
          DBIO.successful(s)
        }
      }
  }

  def getSchemasForDatasetTemplate(
    datasetTemplateId: DatasetTemplateId
  )(implicit
    ec: ExecutionContext
  ): Query[TemplateSchemaTable, TemplateSchemaRecord, Seq] = {
    notDeleted
      .filter(r => r.datasetTemplateId === datasetTemplateId)
  }

  def deleteSchema(
    oId: OrganizationId,
    id: ModelId
  )(implicit
    ec: ExecutionContext
  ): DBIO[Boolean] = {
    val q = for {
      s <- TemplateSchemaMapper
      if s.id === id && s.organizationId === oId && !s.deleted
    } yield s.deleted
    q.update(true).flatMap {
      case 0 => DBIO.failed(InvalidSchemaId(id))
      case _ => DBIO.successful(true)
    }
  }

  // Delete all schemas associated with a given dataset template id
  def deleteDatasetTemplateSchemas(
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

  implicit val getResult = GetResult(t => {
    TemplateSchemaRecord(
      t.nextSchemaId, // id
      t.nextOrganizationId, // org_id
      t.nextString, // schema
      t.nextString, // name
      t.nextString, // displayName
      t.nextString, // description
      t.nextStringOption, // category
      t.nextJson, // properties
      t.nextListString, // required
      t.nextOffsetDateTime, // createdAt
      t.nextBoolean, // deleted,
      t.nextIcon, // icon
      t.nextSchemaIdOption, // parentId
      t.nextDatasetTemplateIdOption // datasetTemplateId
    )
  })

  /**
    * This will fetch all model (schema) "gallery" templates associated with an organization. Schema templates are
    * considered owned by an organization and not part of a published dataset *if*
    * @param oId
    * @return
    */
  def getTemplateGallerySchemasForOrganization(oId: OrganizationId) = {
    sql"""
      WITH inner_q AS (
        SELECT s.*,
          ROW_NUMBER() OVER(PARTITION BY s.name ORDER BY s.created_at DESC) AS pc
        FROM template_schema s WHERE s.org_id = $oId AND s.dataset_template_id IS NULL AND s.deleted = FALSE
      ) SELECT s.* FROM inner_q s WHERE s.pc = 1
    """.as[TemplateSchemaRecord]
  }
}
