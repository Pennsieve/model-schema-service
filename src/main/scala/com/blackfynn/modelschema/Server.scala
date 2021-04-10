package com.blackfynn.modelschema

import collection.JavaConverters._
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.util.Timeout
import com.blackfynn.auth.middleware.Jwt
import com.blackfynn.modelschema.Server.config
import com.blackfynn.modelschema.clients.{ Bioportal, BranchContainer }
import com.blackfynn.modelschema.db.PostgresProfile.api.Database
import com.blackfynn.service.utilities._
import com.typesafe.config.{ Config, ConfigFactory }
import net.ceedubs.ficus.Ficus._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

case class ClientContainer(
  config: Config,
  modelServiceHttp: HttpResponder,
  http: HttpResponder
) {
  val apiServiceUrl: String = config.as[String]("clients.api_service.url")
}

case class JwtContainer(config: Config) extends Jwt.Config {

  override val key: String = config.as[String]("jwt.key")
}

object Host {
  def stripProtocol(host: String): String =
    host.replace("https://", "").replace("http://", "")

  def explode(host: String): (String, Option[Int]) = {
    val parts = host.split(":")
    if (parts.length > 1) {
      (parts.head, Some(parts.last.toInt))
    } else if (parts.length == 1) {
      (parts.head, None)
    } else {
      throw new Exception(s"Bad host: ${host}")
    }
  }
}

case class PostgresDatabase(
  host: String,
  port: Int,
  database: String,
  user: String,
  password: String,
  useSSL: Boolean = true
) {
  private val jdbcBaseURL: String = s"jdbc:postgresql://$host:$port/$database"
  final val jdbcURL = {
    if (useSSL) jdbcBaseURL + "?ssl=true&sslmode=verify-ca"
    else jdbcBaseURL
  }

  val forURL: Database =
    Database.forURL(
      url = jdbcURL,
      user = user,
      password = password,
      driver = "org.postgresql.Driver"
    )
}

class Container(
  val config: Config,
  val modelServiceHttp: HttpResponder,
  val http: HttpResponder,
  val branches: BranchContainer,
  postgresUseSSL: Boolean = true
) {

  val dbConfig = PostgresDatabase(
    host = config.as[String]("postgres.host"),
    port = config.as[Int]("postgres.port"),
    database = config.as[String]("postgres.database"),
    user = config.as[String]("postgres.user"),
    password = config.as[String]("postgres.password"),
    useSSL = postgresUseSSL
  )
  val db = dbConfig.forURL
  val jwt = JwtContainer(config)
  val client = ClientContainer(config, modelServiceHttp, http)
  val prefixUrl = config.as[String]("prefixUrl")
  val log: ContextLogger = new ContextLogger()
}

object Migrate {

  def apply()(implicit msContainer: Container): Unit = {
    val dbConfig = msContainer.dbConfig
    val migrator =
      new MigrationRunner(dbConfig.jdbcURL, dbConfig.user, dbConfig.password)

    val (count, _) = migrator.run()
    msContainer.log.noContext
      .info(s"Migrated $count migrations on ${dbConfig.jdbcURL}")
  }
}

final case class ConsoleEnv(
  system: ActorSystem,
  executionContext: ExecutionContext,
  materializer: ActorMaterializer,
  timeout: Timeout,
  config: Config,
  msContainer: Container
)

object ConsoleEnv {
  def setup(cfg: Config): ConsoleEnv = {

    implicit val system: ActorSystem = ActorSystem("modelSchemaServer")
    implicit val executionContext: ExecutionContext = system.dispatcher
    implicit val materializer: ActorMaterializer = ActorMaterializer()
    implicit lazy val timeout: Timeout = Timeout(5.seconds)

    // implicit val config: Config = ConfigFactory.load("console.conf")
    implicit val config: Config = cfg

    implicit val msContainer: Container = {
      val modelServiceUrl = config.getString("clients.model_service.url")
      val useHttps = modelServiceUrl.startsWith("https")
      val (host, port) = Host.explode(Host.stripProtocol(modelServiceUrl))
      new Container(
        config,
        QueueHttpResponder(
          host = host,
          port = port,
          queueSize = 10000,
          rateLimit = config.getInt("clients.model_service.rate_limit"),
          https = useHttps
        ),
        new SingleHttpResponder,
        BranchContainer(Bioportal.default)
      )
    }

    Migrate()

    ConsoleEnv(
      system,
      executionContext,
      materializer,
      timeout,
      config,
      msContainer
    )
  }
}

object Server extends App {

  implicit val system: ActorSystem = ActorSystem("modelSchemaServer")
  implicit val executionContext: ExecutionContext = system.dispatcher
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  implicit lazy val timeout = Timeout(5.seconds)

  implicit val config: Config = ConfigFactory.load()
  implicit val msContainer: Container = {
    val modelServiceUrl = config.getString("clients.model_service.url")
    val useHttps = modelServiceUrl.startsWith("https")
    val (host, port) = Host.explode(Host.stripProtocol(modelServiceUrl))
    new Container(
      config,
      QueueHttpResponder(
        // we just need the host sans protocol here:
        host = host,
        port = port,
        queueSize = 10000,
        rateLimit = config.getInt("clients.model_service.rate_limit"),
        https = useHttps
      ),
      new SingleHttpResponder,
      BranchContainer(Bioportal.default)
    )
  }

  Migrate()

  lazy val routes: Route = Route.seal(
    HealthcheckHandler.routes ~ ValidateHandler.routes ~ TemplatesHandler.secureRoutes ~ DatasetTemplatesHandler.secureRoutes
  )

  Http()
    .bindAndHandle(routes, "0.0.0.0", 8080)
    .flatMap { binding =>
      val localAddress = binding.localAddress
      val apiServiceUrl = config.getString("clients.api_service.url")
      val modelServiceUrl = config.getString("clients.model_service.url")
      msContainer.log.noContext.info(
        s"Server online at http://${localAddress.getHostName}:${localAddress.getPort}"
          + s"\n- api_service.url = ${apiServiceUrl}"
          + s"\n- model_service.url = ${modelServiceUrl}"
      )

      binding.whenTerminated
        .map { terminated =>
          system.terminate()
          msContainer.log.noContext
            .info(s"Server has terminated with $terminated")
        }
    }
}
