package com.logikujo.www
import unfiltered.response._
import unfiltered.request._
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}

/**
 *
 * UnfilteredM / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www 8/06/14 :: 15:36 :: eof
 *
 */

/*
 * Basic Auth for Async Responses
 */
object Auth {
  def defaultFail(realm: String) = Unauthorized ~> WWWAuthenticate("""Basic realm="%s"""" format realm)
  def basic[A,B](is: (String, String) => Boolean, realm: String = "secret")(
    intent: unfiltered.Async.Intent[HttpServletRequest,HttpServletResponse],
    onFail: ResponseFunction[HttpServletResponse] = defaultFail(realm)): unfiltered.Async.Intent[HttpServletRequest, HttpServletResponse] = {
      case req@BasicAuth(u, p) if (is(u, p)) => intent(req)
      case req@_ => req.respond(onFail)
   }
}
