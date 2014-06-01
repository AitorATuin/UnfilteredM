package com.logikujo.www

import unfiltered.response._
import unfiltered.request._

/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.error 11/05/14 :: 20:37 :: eof
 *
 */
package object error {
  trait ServerResponse[A] extends Responder[A]
  case class ServerResponse500(val msg: String) extends ServerResponse[Any] {
    def respond(res: HttpResponse[Any]): Unit =
      InternalServerError ~> ResponseString(msg)
  }
  val InternalServerError = ServerResponse500("Internal Server Error")
}
