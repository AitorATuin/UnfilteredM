package com.logikujo.www.error

import scalaz.{-\/, \/-, \/}
import argonaut.Json
import argonaut.Argonaut._
import unfiltered.response.{HttpResponse, Status, ResponseString, JsonContent}

/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.error 29/05/14 :: 11:23 :: eof
 *
 */

protected trait JsonResponse extends ServerResponse[Any] {
  val APIStatus: String
  val data: \/[Json,Json]
  lazy val response =
    ("status" := APIStatus) ->:
      ("data" := data.|(jString("null"))) ->:
      ("message" := data.swap.|(jString("null"))) ->:
      jEmptyObject
  lazy val responseF = JsonContent.~>(status).~>(ResponseString(response.toString))
  val status: Status
  def respond(res: HttpResponse[Any]): Unit = responseF(res)
}

sealed case class JsonResponseSuccess(msg: Json, status:Status) extends JsonResponse {
  val APIStatus = "success"
  val data = \/-(msg)
}
sealed case class JsonResponseFail(msg: Json, status: Status) extends JsonResponse {
  val APIStatus = "fail"
  val data = \/-(msg)
}
sealed case class JsonResponseError(msg: Json, status: Status) extends JsonResponse {
  val APIStatus = "error"
  val data = -\/(msg)
}

object JsonResponse {
  val InternalServerError = JsonResponseError(
    jString("Intertal Server Error"),
    unfiltered.response.InternalServerError)
}