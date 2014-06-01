package com.logikujo.www

import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import unfiltered.response._
import unfiltered.request._
import unfiltered.filter._
import unfiltered.directives._
import Directive._
import scala.concurrent.Future
import scala.util.Try

import scala.concurrent.ExecutionContext.Implicits.global

/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www 1/06/14 :: 18:10 :: eof
 *
 */
object AsyncDirective {
  type AsyncDirectiveResponse[A,B] = (HttpRequest[A] => Result[ResponseFunction[B], AsyncResponse[B]])
    type AsyncRequest[A,B] = HttpRequest[A] with unfiltered.Async.Responder[B]
    trait AsyncResponse[A] {
      val future: Future[A]
      val onComplete: Try[A] => ResponseFunction[A]

      def !(f: ResponseFunction[A] => Unit) = future.onComplete(onComplete.andThen(f))
    }

  object AsyncResponse {
    def apply[A](f: Future[A])(func: Try[A] => ResponseFunction[A])  = new AsyncResponse[A] {
      val future = f
      val onComplete = func
    }
  }

  def apply[A, B](intent: PartialFunction[HttpRequest[A], AsyncDirectiveResponse[A,B]]): async.Plan.Intent = {
    case req: AsyncRequest[A,B] if intent.isDefinedAt(req) => intent(req)(req) match {
      case Result.Success(asyncResponse) => asyncResponse ! (req.respond _)
      case Result.Failure(response) => req.respond(response)
      case Result.Error(response) => req.respond(response)
    }
  }
}
