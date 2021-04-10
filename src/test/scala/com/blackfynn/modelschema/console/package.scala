package com.blackfynn.modelschema.console

import java.time.ZoneOffset

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ HttpHeader, StatusCodes }
import akka.stream.ActorMaterializer
import com.blackfynn.auth.middleware.Jwt
import com.blackfynn.modelschema.{
  responses,
  ClientContainer,
  ConsoleEnv,
  Container,
  InvalidModelId,
  ModelServiceJsonError
}
import com.blackfynn.modelschema.clients.ModelService
import com.blackfynn.modelschema.db.DatasetTemplateMapper
import com.blackfynn.modelschema.managers.DatasetTemplateManager
import com.blackfynn.modelschema.managers.DatasetTemplateManager.PersistResult
import com.blackfynn.modelschema.model.{
  DatasetId,
  DatasetTemplate,
  DatasetTemplateId,
  DatasetTemplateSchema,
  ModelId,
  OrganizationId
}
import com.blackfynn.modelschema.responses.modelservice.{
  DatasetSchemaResponse,
  ModelResponse
}
import com.blackfynn.modelschema.util.{ Generators, HttpUtils }
import com.typesafe.config.ConfigFactory

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration._

/**
  * Sample usage:
  *
  * // Generate a random dataset
  * val bfOrg = 5
  * val initialDataset = 9074
  * val publishTemplateToDataset = 9086
  * val token = "60dd7abc-c584-4a32-92b6-3e89d3246e7e"
  * val template = Console.createTemplate(bfOrg, initialDataset, "My Test Template", "Model of Something")
  * val somethingBig = Console.generateSchema(maxModels = 25, maxProps = 25, maxRelationships = 25, maxLinkedProps = 25)
  * Console.saveSchemaToTemplate(template, somethingBig)
  * Console.publishSchema(template.id.value, publishTemplateToDataset, token)
  */
object Console {

  val traceId = "9999-9999"

  def setup: ConsoleEnv = {
    val config = ConfigFactory.load()
    ConsoleEnv.setup(config)
  }

  def generateSchema(
    minModels: Int = 3,
    maxModels: Int = 4,
    minProps: Int = 3,
    maxProps: Int = 5,
    minRelationships: Int = 3,
    maxRelationships: Int = 5,
    minLinkedProps: Int = 3,
    maxLinkedProps: Int = 5
  ): DatasetSchemaResponse =
    Generators.randomDatasetSchema(
      (minModels, maxModels),
      (minProps, maxProps),
      (minRelationships, maxRelationships),
      (minLinkedProps, maxLinkedProps)
    )

  def ingestDataset(
    datasetId: Int,
    token: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): DatasetSchemaResponse = {
    val fut =
      ModelService.ingestDatasetSchema(
        DatasetId(datasetId),
        Jwt.Token(token),
        traceId
      )
    Await.result(fut, 5.seconds)
  }

  def getModel(
    modelId: String,
    datasetId: Int,
    token: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): ModelResponse = {
    val fut =
      ModelService.getModel(
        ModelId(modelId),
        DatasetId(datasetId),
        Jwt.Token(token),
        traceId
      )
    Await.result(fut, 5.seconds)
  }

  def getModels(
    datasetId: Int,
    token: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): Seq[ModelResponse] = {
    val fut =
      ModelService.getModels(DatasetId(datasetId), Jwt.Token(token), traceId)
    Await.result(fut, 5.seconds)
  }

  def createTemplate(
    orgId: Int,
    datasetId: Int,
    name: String,
    description: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): DatasetTemplate = {
    val template = DatasetTemplate(
      id = DatasetTemplateId(0),
      organizationId = OrganizationId(orgId),
      datasetId = DatasetId(datasetId),
      name = name,
      description = description
    )
    Await.result(
      msContainer.db.run(DatasetTemplateMapper.insert(template)),
      5.seconds
    )
  }

  def getTemplate(
    templateId: Int
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): DatasetTemplate = {
    Await.result(
      DatasetTemplateManager.getTemplate(DatasetTemplateId(templateId)),
      5.seconds
    )
  }

  def saveSchemaToTemplate(
    template: DatasetTemplate,
    datasetSchema: DatasetSchemaResponse
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): PersistResult = {
    val templateSchema: DatasetTemplateSchema =
      DatasetTemplateManager.remapSchemaObjects(
        template.organizationId,
        template.id,
        template.name,
        template.description
      )(datasetSchema)
    val result: Future[PersistResult] =
      DatasetTemplateManager.persistSchemaObjects(templateSchema)
    Await.result(result, 5.seconds)
  }

  def getTemplateSchema(
    datasetTemplateId: Int
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): DatasetTemplateSchema = {
    val schema: Future[DatasetTemplateSchema] =
      DatasetTemplateManager.getTemplateSchema(
        DatasetTemplateId(datasetTemplateId)
      )
    Await.result(schema, 5.seconds)
  }

  def publishSchema(
    datasetTemplateId: Int,
    name: String,
    description: Option[String],
    token: String
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    msContainer: Container
  ): DatasetTemplateManager.PublishResult = {
    val templateId = DatasetTemplateId(datasetTemplateId)
    val fut = for {
      schema <- DatasetTemplateManager.getTemplateSchema(templateId)
      claim <- Jwt
        .parseClaim(Jwt.Token(token))(msContainer.jwt)
        .fold(Future.failed, Future.successful)
      publishResult <- DatasetTemplateManager.publishSchema(
        schema,
        name,
        description,
        claim,
        traceId
      )
    } yield publishResult
    Await.result(fut, 30.seconds)
  }

}
