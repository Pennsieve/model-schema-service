package com.blackfynn.modelschema.util

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{
  Authorization,
  ModeledCustomHeader,
  ModeledCustomHeaderCompanion,
  OAuth2BearerToken
}
import akka.http.scaladsl.server.Directives.{ complete, onComplete }
import akka.http.scaladsl.server.Route
import akka.stream.ActorMaterializer
import akka.util.ByteString
import com.blackfynn.auth.middleware.Jwt
import com.blackfynn.modelschema.ErrorResponse
import com.blackfynn.service.utilities.{ ContextLogger, Http, HttpResponder }
import io.circe.parser.parse
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, Json }

import scala.concurrent.duration._
import scala.concurrent.{ ExecutionContext, Future }
import scala.util.{ Failure, Success, Try }

final class TraceIdHeader(token: String)
    extends ModeledCustomHeader[TraceIdHeader] {
  override def renderInRequests = true
  override def renderInResponses = true
  override val companion = TraceIdHeader
  override def value: String = token
}

object TraceIdHeader extends ModeledCustomHeaderCompanion[TraceIdHeader] {
  override val name = "X-Bf-Trace-Id"
  override def parse(value: String) = Try(new TraceIdHeader(value))
}

object HttpUtils extends Http(30.seconds) {

  def authHeader(token: Jwt.Token) =
    Authorization(OAuth2BearerToken(token.value))

  def traceIdHeader(traceId: String) = TraceIdHeader(traceId)

  /**
    * AS default error handler that will return a 500: ModelServiceJsonError error
    */
  val defaultOnError: (StatusCode, Json) => Future[HttpResponse] =
    (code: StatusCode, json: Json) => {
      Future.successful(HttpResponse(code, entity = json.noSpaces))
    }

  def doRequestReturningBody[IN: Encoder](
    method: HttpMethod,
    headers: List[HttpHeader],
    uri: String,
    payload: Option[IN],
    responder: HttpResponder.Responder,
    onError: (StatusCode, Json) => Future[HttpResponse]
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    log: ContextLogger
  ): Future[HttpResponse] = {
    val request = HttpRequest(
      method = method,
      uri = uri,
      entity = payload match {
        case Some(p) =>
          HttpEntity.Strict(
            ContentType(MediaTypes.`application/json`),
            ByteString(p.asJson.noSpaces)
          )
        case None => HttpEntity.Empty
      },
      headers = headers
    )
    responder(request).flatMap { response: HttpResponse =>
      if (response.status.isSuccess) {
        Future.successful(response)
      } else {
        getBody(response).flatMap { body: String =>
          {
            log.noContext.error(
              s"$method {$uri}:${payload.asJson.noSpaces} => $body"
            )
            parse(body) match {
              case Left(_) =>
                Future
                  .successful(HttpResponse(response.status, entity = body))
              case Right(json) => onError(response.status, json)
            }
          }
        }
      }
    }
  }

  def doRequestReturningJson[IN: Encoder](
    method: HttpMethod,
    headers: List[HttpHeader],
    uri: String,
    payload: Option[IN],
    responder: HttpResponder.Responder,
    onError: (StatusCode, Json) => Future[HttpResponse]
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    log: ContextLogger
  ): Future[Json] =
    doRequestReturningBody(method, headers, uri, payload, responder, onError)
      .flatMap { response: HttpResponse =>
        {
          getBody(response).flatMap { body: String =>
            parse(body) match {
              case Left(failure) => Future.failed(failure)
              case Right(json) => Future.successful(json)
            }
          }
        }
      }

  def doRequest[IN: Encoder, OUT: Decoder](
    method: HttpMethod,
    headers: List[HttpHeader],
    uri: String,
    payload: Option[IN],
    responder: HttpResponder.Responder,
    onError: (StatusCode, Json) => Future[HttpResponse]
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    log: ContextLogger
  ): Future[OUT] =
    doRequestReturningBody(method, headers, uri, payload, responder, onError)
      .flatMap { response: HttpResponse =>
        HttpUtils.unmarshallAs[OUT](response)
      }

  def get[OUT: Decoder](
    headers: List[HttpHeader],
    uri: String,
    responder: HttpResponder.Responder,
    onError: (StatusCode, Json) => Future[HttpResponse] = defaultOnError
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    log: ContextLogger
  ): Future[OUT] =
    doRequest[Unit, OUT](
      HttpMethods.GET,
      headers,
      uri,
      None,
      responder,
      onError
    )

  def getReturningJson(
    headers: List[HttpHeader],
    uri: String,
    responder: HttpResponder.Responder,
    onError: (StatusCode, Json) => Future[HttpResponse] = defaultOnError
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    log: ContextLogger
  ): Future[Json] =
    doRequestReturningJson[Unit](
      HttpMethods.GET,
      headers,
      uri,
      None,
      responder,
      onError
    )

  def post[IN: Encoder, OUT: Decoder](
    headers: List[HttpHeader],
    uri: String,
    payload: IN,
    responder: HttpResponder.Responder,
    onError: (StatusCode, Json) => Future[HttpResponse] = defaultOnError
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    log: ContextLogger
  ): Future[OUT] =
    doRequest[IN, OUT](
      HttpMethods.POST,
      headers,
      uri,
      Some(payload),
      responder,
      onError
    )

  def postReturningJson[IN: Encoder](
    headers: List[HttpHeader],
    uri: String,
    payload: IN,
    responder: HttpResponder.Responder,
    onError: (StatusCode, Json) => Future[HttpResponse] = defaultOnError
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    log: ContextLogger
  ): Future[Json] =
    doRequestReturningJson[IN](
      HttpMethods.POST,
      headers,
      uri,
      Some(payload),
      responder,
      onError
    )

  def put[IN: Encoder, OUT: Decoder](
    headers: List[HttpHeader],
    uri: String,
    payload: IN,
    responder: HttpResponder.Responder,
    onError: (StatusCode, Json) => Future[HttpResponse] = defaultOnError
  )(implicit
    system: ActorSystem,
    ec: ExecutionContext,
    mat: ActorMaterializer,
    log: ContextLogger
  ): Future[OUT] =
    doRequest[IN, OUT](
      HttpMethods.PUT,
      headers,
      uri,
      Some(payload),
      responder,
      onError
    )
}

object CompleteFutureImplicits {

  implicit class CompleteFuture[T: ToResponseMarshaller](f: Future[T]) {
    def completeFuture: Route = {
      onComplete(f) {
        case Success(v) => complete(v)
        case Failure(e: Throwable) => complete(ErrorResponse.from(e))
      }
    }
  }
}
