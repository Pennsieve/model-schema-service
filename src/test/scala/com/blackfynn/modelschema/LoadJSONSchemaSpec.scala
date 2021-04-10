package com.blackfynn.modelschema

import akka.actor.ActorSystem
import akka.http.scaladsl.{ Http => HttpClient }
import akka.http.scaladsl.model.{ HttpRequest, HttpResponse, StatusCodes }
import akka.stream.ActorMaterializer
import com.blackfynn.modelschema.managers.SchemaManager
import com.blackfynn.modelschema.model._
import com.blackfynn.modelschema.model.implicits._
import com.blackfynn.modelschema.util.MockHttpResponder
import com.blackfynn.service.utilities.MigrationRunner
import org.scalatest.concurrent.ScalaFutures
import io.circe._
import io.circe.parser._
import io.circe.syntax._

import scala.concurrent.{ Await, ExecutionContext, Future }
import scala.concurrent.duration._
import scala.io.Source

class EndToEndRealResponder(
  implicit
  val system: ActorSystem,
  val executionContext: ExecutionContext,
  val materializer: ActorMaterializer
) extends MockHttpResponder {
  override def responder: HttpRequest => Future[HttpResponse] =
    (req: HttpRequest) => {
      HttpClient().singleRequest(req)
    }

  override def mock = {
    case (_, _) => (StatusCodes.OK, "OK".asJson)
  }
}

class LoadJSONSchemaSpec extends HelperSpec with ScalaFutures {

  override val responder =
    new EndToEndRealResponder()(system, executionContext, materializer)

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

  def loadSchemaFile(schemaFileName: String): Future[TemplateSchemaRecord] = {
    val schema = Source
      .fromURL(
        getClass
          .getResource("/json-definition-files/" + schemaFileName + ".json")
      )
      .mkString
    for {
      createSchemaRequest <- decode[CreateSchemaRequest](schema) match {
        case Left(err) => Future.failed(err)
        case Right(request) => Future.successful(request)
      }
      _ <- SchemaManager.isValid(createSchemaRequest)
      templateRecord <- SchemaManager.create(
        OrganizationId(1),
        createSchemaRequest
      )
    } yield templateRecord
  }

  val waitFor = 10.seconds

  "Loading non-Bioportal schema JSON files" should {
    "work for acquisition.json" in {
      val acquisition: TemplateSchemaRecord =
        Await.result(loadSchemaFile("acquisition"), waitFor)
      acquisition.name should ===("acquisition")
      acquisition.description should ===(
        "A representation of an acquisition of data by a recording device"
      )
      acquisition.category should ===(Some("Acquisition"))
      acquisition.required should ===(List.empty)
    }

    "work for intervention.json" in {
      val intervention: TemplateSchemaRecord =
        Await.result(loadSchemaFile("intervention"), waitFor)
      intervention.name should ===("intervention")
      intervention.description should ===("This model defines an intervention")
      intervention.category should ===(Some("Protocol"))
      intervention.required should contain("Event or stressor description")
      intervention.required should contain("Start time")
      intervention.required should contain("Stop time")
    }

    "work for sample_protocol.json" in {
      val sampleProtocol: TemplateSchemaRecord =
        Await.result(loadSchemaFile("sample_protocol"), waitFor)
      sampleProtocol.name should ===("sample_protocol")
      sampleProtocol.description should ===(
        "Sample protocol based on the Nature Protocols schema."
      )
      sampleProtocol.category should ===(None)
      sampleProtocol.required should contain("abstract")
      sampleProtocol.required should contain("introduction")
      sampleProtocol.required should contain("procedure")
    }

    "work for sedation.json" in {
      val sedation: TemplateSchemaRecord =
        Await.result(loadSchemaFile("sedation"), waitFor)
      sedation.name should ===("sedation_information")
      sedation.description should be(
        "This model provides information about the sedation procedure"
      )
      sedation.category should ===(Some("None"))
      sedation.required should contain("Sedation Source")
      sedation.required should contain("Sedation Vehicle")
    }

    "work for specimen.json" in {
      val specimen: TemplateSchemaRecord =
        Await.result(loadSchemaFile("specimen"), waitFor)
      specimen.name should ===("specimen")
      specimen.description should be(
        "A representation of a specimen (human, animal, or tissue sample)"
      )
      specimen.category should ===(Some("Specimen"))
      specimen.required should ===(List.empty)
    }

    "work for subject.json" in {
      val subject: TemplateSchemaRecord =
        Await.result(loadSchemaFile("subject"), waitFor)
      subject.name should ===("sample_subject")
      subject.description should be("Sample subject model.")
      subject.category should ===(None)
      subject.required should contain("name")
      subject.required should contain("sex")
      subject.required should contain("species")
    }
  }
}
