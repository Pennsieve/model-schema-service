package com.blackfynn.modelschema

import scala.language.postfixOps
import java.util.UUID

import scala.concurrent.{ Await, ExecutionContext }
import scala.concurrent.duration._
import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{ Authorization, OAuth2BearerToken }
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.Unmarshaller._
import akka.stream.ActorMaterializer
import com.blackfynn.modelschema.db.PostgresProfile.api._
import com.blackfynn.modelschema.model.implicits._
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.model.implicits._
import com.blackfynn.modelschema.{ responses => res }
import com.blackfynn.modelschema.util.{ Generators, MockHttpResponder, Token }
import com.blackfynn.http.server.definitions
import com.blackfynn.service.utilities.MigrationRunner
import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import akka.http.scaladsl.unmarshalling.Unmarshaller._
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{ Authorization, OAuth2BearerToken }
import akka.http.scaladsl.server.Route
import com.blackfynn.modelschema.db.{
  DatasetTemplateMapper,
  TemplateSchemaLinkedPropertyMapper,
  TemplateSchemaMapper,
  TemplateSchemaRelationshipMapper
}
import com.blackfynn.modelschema.managers.DatasetTemplateManager
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.syntax._
import io.circe._

import scala.concurrent.duration._
import scala.concurrent.{ Await, ExecutionContext }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.Matchers._

class DatasetTemplateManagerMockResponder(
  implicit
  val system: ActorSystem,
  val executionContext: ExecutionContext,
  val materializer: ActorMaterializer
) extends MockHttpResponder {
  override def mock = {
    case (_, _) =>
      (
        StatusCodes.OK,
        (Seq.empty: Seq[responses.modelservice.SchemaPropertyResponse]).asJson
      )
  }
}

class DatasetTemplateManagerSpec extends HelperSpec with ScalaFutures {

  override val responder =
    new DatasetTemplatesHandlerMockResponder()(
      system,
      executionContext,
      materializer
    )

  override def afterStart(): Unit = {
    super.afterStart

    val migrator = new MigrationRunner(
      container.jdbcUrl,
      container.username,
      container.password
    )
    val (count, _) = migrator.run()
    assert(count > 0)
  }

  "DatasetTemplateManager" should {

    "remap schema objects" in {

      val orgId = Generators.randomOrganizationId
      val datasetTemplateId = Generators.randomDatasetTemplateId

      // Generate a test dataset schema, as provided through the model (concept) service:
      val oldDatasetSchema: res.modelservice.DatasetSchemaResponse =
        Generators.randomDatasetSchema((4, 4), (3, 5), (3, 5), (3, 5))

      // Old IDs should map from models -> { linked properties | relationships }
      val oldModelIds: Set[UUID] = oldDatasetSchema.modelIds
      val oldRelationshipLinks: Set[UUID] =
        oldDatasetSchema.relationships.flatMap { r =>
          r.modelIds
        }.toSet
      val oldLinkedPropertyLinks: Set[UUID] =
        oldDatasetSchema.linkedProperties.flatMap {
          lp: res.modelservice.LinkedPropertyTargetResponse =>
            lp.link.modelIds
        }.toSet

      oldModelIds should not be empty
      oldRelationshipLinks.subsetOf(oldModelIds) should be(true)
      oldLinkedPropertyLinks.subsetOf(oldModelIds) should be(true)

      // Remap the model IDs:
      val newDatasetSchema: DatasetTemplateSchema =
        DatasetTemplateManager.remapSchemaObjects(
          orgId,
          datasetTemplateId,
          "remap-test",
          "Remap test"
        )(oldDatasetSchema)

      val newModelIds: Set[ModelId] = newDatasetSchema.modelIds
      val newRelationshipLinks: Set[ModelId] =
        newDatasetSchema.relationships.flatMap { r =>
          r.modelIds
        }.toSet
      val newLinkedPropertyLinks: Set[ModelId] =
        newDatasetSchema.linkedProperties.flatMap {
          lp: TemplateSchemaLinkedProperty =>
            lp.modelIds
        }.toSet

      newModelIds should not be empty
      // No old IDs should appear anywhere in the remapped dataset schema:
      newRelationshipLinks.subsetOf(newModelIds) should be(true)
      newLinkedPropertyLinks.subsetOf(newModelIds) should be(true)
    }
  }

  "remaps and persistents a dataset schema" in {

    val orgId = Generators.randomOrganizationId
    val datasetId = Generators.randomDatasetId

    // Create a dataset template:
    val dsTemplate: DatasetTemplate =
      Await
        .result(
          msContainer.db.run(
            DatasetTemplateMapper.insert(
              DatasetTemplate(
                id = DatasetTemplateId(0),
                organizationId = orgId,
                datasetId = datasetId,
                name = "remap-dataset-template",
                description = s"example template"
              )
            )
          ),
          5.seconds
        )

    val oldDatasetSchema: res.modelservice.DatasetSchemaResponse =
      Generators.randomDatasetSchema((25, 25), (25, 25), (25, 25), (25, 25))

    val newDatasetSchema: DatasetTemplateSchema =
      DatasetTemplateManager.remapSchemaObjects(
        orgId,
        dsTemplate.id,
        "NEW-AND-IMPROVED-remap-and-persist-test",
        "Remap and persist test"
      )(oldDatasetSchema)

    val saveSchema =
      DatasetTemplateManager.persistSchemaObjects(newDatasetSchema)

    val persistResult = Await.result(saveSchema, 5 seconds)

    persistResult.modelCount should be(
      oldDatasetSchema.modelsAndProperties.size
    )
    persistResult.relationshipsCount should be(
      oldDatasetSchema.relationships.size
    )
    persistResult.linkedPropertyCount should be(
      oldDatasetSchema.linkedProperties.size
    )

    // Fetch results:

    // models
    Await
      .result(
        msContainer.db
          .run(
            TemplateSchemaMapper
              .getSchemasForDatasetTemplate(dsTemplate.id)
              .result
          ),
        5 seconds
      )
      .size should be(oldDatasetSchema.modelsAndProperties.size)

    // relationships
    Await
      .result(
        msContainer.db.run(
          TemplateSchemaRelationshipMapper
            .getRelationshipsForDatasetTemplate(dsTemplate.id)
            .result
        ),
        5 seconds
      )
      .size should be(oldDatasetSchema.relationships.size)

    // linked properties
    Await
      .result(
        msContainer.db.run(
          TemplateSchemaLinkedPropertyMapper
            .getLinkedPropertiesForDatasetTemplate(dsTemplate.id)
            .result
        ),
        5 seconds
      )
      .size should be(oldDatasetSchema.linkedProperties.size)
  }
}
