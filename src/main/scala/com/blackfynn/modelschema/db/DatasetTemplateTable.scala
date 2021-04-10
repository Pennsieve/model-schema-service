package com.blackfynn.modelschema.db

import java.time.{ OffsetDateTime, ZoneOffset }

import com.blackfynn.http.server.definitions
import com.blackfynn.modelschema.{
  db,
  DuplicateDatasetTemplateName,
  InvalidDatasetTemplateId
}
import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.model._
import slick.dbio.Effect
import slick.jdbc.{ GetResult, PositionedResult }
import slick.sql.FixedSqlAction

import scala.concurrent.ExecutionContext

final class DatasetTemplateTable(tag: Tag)
    extends Table[DatasetTemplate](tag, Some("public"), "dataset_template") {
  def id = column[DatasetTemplateId]("id", O.PrimaryKey, O.AutoInc)
  def organizationId = column[OrganizationId]("organization_id")
  def datasetId = column[DatasetId]("dataset_id")
  def name = column[String]("name")
  def description = column[String]("description")
  def createdAt = column[OffsetDateTime]("created_at")
  def updatedAt = column[OffsetDateTime]("updated_at")
  def deleted = column[Boolean]("deleted")

  def * =
    (
      organizationId,
      datasetId,
      name,
      description,
      createdAt,
      updatedAt,
      deleted,
      id
    ).mapTo[DatasetTemplate]
}

object DatasetTemplateTable {

  implicit class PgPositionedResult(val r: PositionedResult) extends AnyVal {
    def nextDatasetTemplateId: DatasetTemplateId =
      DatasetTemplateId(r.nextInt)
    def nextOrganizationId: OrganizationId = OrganizationId(r.nextInt)
    def nextDatasetId: DatasetId = DatasetId(r.nextInt)
    def nextOffsetDateTime: OffsetDateTime =
      OffsetDateTime.ofInstant(r.nextTimestamp.toInstant, ZoneOffset.UTC)
  }
}

object DatasetTemplateMapper extends TableQuery(new DatasetTemplateTable(_)) {

  import DatasetTemplateTable._

  def notDeleted: Query[DatasetTemplateTable, DatasetTemplate, Seq] =
    this.filter(!_.deleted)

  def insert(record: DatasetTemplate) =
    this returning this.map(_.id) into ((item, id) => item.copy(id = id)) += record

  implicit val getResult = GetResult(t => {
    DatasetTemplate(
      t.nextOrganizationId, // organizationId
      t.nextDatasetId, // datasetId
      t.nextString, // name
      t.nextString, // description
      t.nextOffsetDateTime, // createdAt
      t.nextOffsetDateTime, // updatedAt
      t.nextBoolean, // deleted
      t.nextDatasetTemplateId // id
    )
  })

  def checkIfNameExistsForDataset(
    organizationId: OrganizationId,
    datasetId: DatasetId,
    name: String
  )(implicit
    ec: ExecutionContext
  ): DBIO[Boolean] =
    notDeleted
      .filter(_.name === name)
      .filter(_.organizationId === organizationId)
      .filter(_.datasetId === datasetId)
      .exists
      .result

  def enforceNameUniqueness(
    organizationId: OrganizationId,
    datasetId: DatasetId,
    name: String
  )(implicit
    ec: ExecutionContext
  ): DBIO[Unit] =
    checkIfNameExistsForDataset(organizationId, datasetId, name).flatMap {
      nameExists =>
        {
          if (nameExists) {
            DBIO.failed(DuplicateDatasetTemplateName(datasetId, name))
          } else {
            DBIO.successful(())
          }
        }
    }

  // Retrieve all dataset templates belonging to an organization
  def getDatasetTemplatesForOrganization(
    organizationId: OrganizationId
  ): DBIO[Seq[DatasetTemplate]] =
    notDeleted.filter(_.organizationId === organizationId).result

  // Get a dataset template by ID.
  def getDatasetTemplate(
    templateId: DatasetTemplateId
  )(implicit
    ec: ExecutionContext
  ): DBIO[DatasetTemplate] =
    notDeleted
      .filter(_.id === templateId)
      .result
      .headOption
      .flatMap { template: Option[DatasetTemplate] =>
        {
          template match {
            case None => DBIO.failed(InvalidDatasetTemplateId(templateId))
            case Some(template) => DBIO.successful(template)
          }
        }
      }

  // Retrieve a dataset template's schema by id
  def getDatasetTemplateSchema(
    datasetTemplateId: DatasetTemplateId
  )(implicit
    ec: ExecutionContext
  ): DBIO[DatasetTemplateSchema] =
    for {
      datasetTemplate <- getDatasetTemplate(datasetTemplateId)
      models <- TemplateSchemaMapper.notDeleted
        .filter(r => r.datasetTemplateId === datasetTemplateId)
        .result
      linkedProperties <- TemplateSchemaLinkedPropertyMapper.notDeleted
        .filter(r => r.datasetTemplateId === datasetTemplateId)
        .result
      relationships <- TemplateSchemaRelationshipMapper.notDeleted
        .filter(r => r.datasetTemplateId === datasetTemplateId)
        .result
    } yield
      DatasetTemplateSchema(
        id = datasetTemplateId,
        name = datasetTemplate.name,
        description = datasetTemplate.description,
        models = models,
        relationships = relationships,
        linkedProperties = linkedProperties
      )

  // Delete a dataset template
  def deleteDatasetTemplate(
    datasetTemplateId: DatasetTemplateId
  )(implicit
    ec: ExecutionContext
  ): DBIOAction[Unit, NoStream, Effect.Write] = {
    val deleteQuery = for {
      updateCount <- notDeleted
        .filter(_.id === datasetTemplateId)
        .map(_.deleted)
        .update(true)
    } yield updateCount

    deleteQuery
      .flatMap { count =>
        if (count == 0)
          DBIO.failed(InvalidDatasetTemplateId(datasetTemplateId))
        else
          DBIO.successful(())
      }
  }

  // Update a dataset template name
  def updateDatasetTemplateName(
    id: DatasetTemplateId,
    newName: String
  )(implicit
    ec: ExecutionContext
  ) =
    notDeleted
      .filter(_.id === id)
      .map(_.name)
      .update(newName)

  // Update a dataset template description
  def updateDatasetTemplateDescription(
    id: DatasetTemplateId,
    newDescription: String
  )(implicit
    ec: ExecutionContext
  ) =
    notDeleted
      .filter(_.id === id)
      .map(_.description)
      .update(newDescription)
}
