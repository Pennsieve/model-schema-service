package com.blackfynn.modelschema

import scala.concurrent.{ ExecutionContext, Future }
import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import com.blackfynn.http.server.definitions
import com.blackfynn.http.server.validate.{
  ValidateResource,
  ValidateHandler => GuardrailHandler
}
import com.blackfynn.modelschema.managers.SchemaManager
import com.blackfynn.modelschema.model.implicits._

class ValidateHandler(
  implicit
  system: ActorSystem,
  executionContext: ExecutionContext,
  materializer: ActorMaterializer,
  msContainer: Container
) extends GuardrailHandler {

  override def validateTemplate(
    respond: ValidateResource.validateTemplateResponse.type
  )(
    templateSchema: definitions.CreateSchemaRequest
  ): scala.concurrent.Future[ValidateResource.validateTemplateResponse] = {
    SchemaManager
      .isValid(templateSchema.toDomainObject)
      .map(_ => respond.OK)
      .recoverWith {
        ErrorResponse.recoverWithFn {
          case e: InvalidSchemaUrl =>
            Future.successful(
              respond.BadRequest(definitions.ValidationBadRequest(e.message))
            )
          case e: SchemaValidationException =>
            Future.successful(
              respond.BadRequest(
                definitions.ValidationBadRequest(
                  e.message,
                  problems = Some(e.payload.toIndexedSeq)
                )
              )
            )
          case e: InvalidModelProperties =>
            Future.successful(
              respond.BadRequest(definitions.ValidationBadRequest(e.message))
            )
          case e: InvalidPropertyNamesFound =>
            Future.successful(
              respond.BadRequest(
                definitions.ValidationBadRequest(
                  e.message,
                  problems = Some(e.names.toIndexedSeq)
                )
              )
            )
          case e @ InvalidBranchPropertiesFound(properties) =>
            Future.successful(
              respond.BadRequest(
                definitions
                  .ValidationBadRequest(e.message, Some(properties.map {
                    case (name, error) => s"$name: $error"
                  }.toIndexedSeq))
              )
            )
          case e: InvalidDefaultValuesFound =>
            Future.successful(
              respond.BadRequest(
                definitions.ValidationBadRequest(
                  e.message,
                  problems = Some(e.names.toIndexedSeq)
                )
              )
            )
        }
      }
  }
}

object ValidateHandler {
  def routes(
    implicit
    system: ActorSystem,
    materializer: ActorMaterializer,
    executionContext: ExecutionContext,
    msContainer: Container
  ): Route = ValidateResource.routes(new ValidateHandler)
}
