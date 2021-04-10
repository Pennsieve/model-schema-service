package com.blackfynn.modelschema

import com.blackfynn.modelschema.util.MockHttpResponder
import scala.concurrent.duration._
import scala.language.postfixOps

import akka.http.scaladsl.testkit.{ RouteTestTimeout, ScalatestRouteTest }
import akka.testkit.TestDuration
import com.blackfynn.modelschema.clients._
import com.dimafeng.testcontainers.{ ForAllTestContainer, PostgreSQLContainer }
import com.typesafe.config.{ Config, ConfigFactory, ConfigValueFactory }
import org.scalatest.{ Matchers, WordSpec }

trait HelperSpec
    extends WordSpec
    with Matchers
    with ForAllTestContainer
    with ScalatestRouteTest {

  val responder: MockHttpResponder

  implicit val executionContext = executor
  implicit val timeout = RouteTestTimeout(5.seconds dilated)

  val container = PostgreSQLContainer()

  val BIOPORTAL_TEST_HOST = "<bioportalhost>"

  lazy val config: Config = {
    ConfigFactory
      .empty()
      .withValue(
        "clients.api_service.url",
        ConfigValueFactory.fromAnyRef("local-api-service")
      )
      .withValue(
        "clients.model_service.url",
        ConfigValueFactory.fromAnyRef("http://localhost:8080")
      )
      .withValue(
        "clients.model_service.rate_limit",
        ConfigValueFactory.fromAnyRef(25)
      )
      .withValue("jwt.key", ConfigValueFactory.fromAnyRef("abc123"))
      .withValue(
        "postgres.host",
        ConfigValueFactory.fromAnyRef(container.containerIpAddress)
      )
      .withValue(
        "postgres.port",
        ConfigValueFactory.fromAnyRef(container.mappedPort(5432))
      )
      .withValue("postgres.database", ConfigValueFactory.fromAnyRef("test"))
      .withValue(
        "postgres.user",
        ConfigValueFactory.fromAnyRef(container.username)
      )
      .withValue(
        "postgres.password",
        ConfigValueFactory.fromAnyRef(container.password)
      )
      .withValue(
        "prefixUrl",
        ConfigValueFactory
          .fromAnyRef("http://localhost:8080/local-model-schema")
      )
      .withValue("bioportal.key", ConfigValueFactory.fromAnyRef("apikey"))
      .withValue(
        "bioportal.host",
        ConfigValueFactory.fromAnyRef(BIOPORTAL_TEST_HOST)
      )
  }

  lazy val bioportal = new Bioportal()(
    system,
    executionContext,
    materializer,
    config,
    new BioportalTestHttpResponder(BIOPORTAL_TEST_HOST)
  )

  lazy implicit val msContainer =
    new Container(
      config,
      responder,
      responder,
      BranchContainer(bioportal),
      postgresUseSSL = false
    )
}
