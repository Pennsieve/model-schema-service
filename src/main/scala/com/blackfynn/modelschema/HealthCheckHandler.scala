package com.blackfynn.modelschema

import akka.http.scaladsl.server.Route

import scala.concurrent.Future
import akka.stream.ActorMaterializer
import com.blackfynn.http.server.healthcheck.{
  HealthcheckResource,
  HealthcheckHandler => GuardrailHandler
}

class HealthcheckHandler extends GuardrailHandler {
  override def healthcheck(
    respond: HealthcheckResource.healthcheckResponse.type
  )(
  ): scala.concurrent.Future[HealthcheckResource.healthcheckResponse] = {
    Future.successful(respond.OK)
  }
}

object HealthcheckHandler {
  def routes(
    implicit
    materializer: ActorMaterializer
  ): Route = HealthcheckResource.routes(new HealthcheckHandler)
}
