package com.blackfynn.modelschema.util

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.{ Http => HttpClient }

import com.blackfynn.service.utilities.{
  MockHttpResponder => BaseMockHttpResponder
}

import com.typesafe.scalalogging.LazyLogging

trait MockHttpResponder extends BaseMockHttpResponder with LazyLogging {

  override def responder = (req: HttpRequest) => {
    if (mock.isDefinedAt((req.method, req.uri.toString))) {
      val (statusCode, payload) = mock((req.method, req.uri.toString))
      jsonResponse(statusCode, payload)
    } else {
      logger.info(
        s"got-mock-request :: proxying request => ${req.method} -> ${req.uri}"
      )
      HttpClient().singleRequest(req)
    }
  }
}
