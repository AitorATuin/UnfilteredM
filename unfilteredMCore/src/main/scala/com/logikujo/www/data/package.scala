package com.logikujo.www

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.util.Try
import com.typesafe.config.Config
import unfiltered.directives._
import Directives._
import dispatch._
import unfiltered.response._
import scalaz._
import Scalaz._
import scala.concurrent.ExecutionContext.Implicits.global
/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com

 * com.logikujo.www.data 8/02/14 :: 17:40 :: eof
 *
 */
package object data {
  // Let scalaz know that ResultM is a monad
  implicit object resultMBind extends Bind[ResultM] {
    def bind[A, B](fa: ResultM[A])(f: A => ResultM[B]): ResultM[B] = fa.flatMap(f)
    def map[A, B](fa: ResultM[A])(f: A => B): ResultM[B] = fa.map(f)
  }

  implicit def configDirective(c: Configuration) = success(c)

  trait FuturableDirective[A,B,C] {
    val v: Future[DirectiveT]

    private type PFResponse = PartialFunction[Throwable, Responder[Any]]
    private type DirectiveT = Directive[A, ResponseFunction[B], C]
    private lazy val result = Try(Await.result(v, 500 millis))
    private lazy val defaultErrorResponse = error.InternalServerError

    def run: DirectiveT = run(defaultErrorResponse)
    def run(errorResponse: Responder[Any]): DirectiveT = result.
      getOrElse(failure(errorResponse))
    def runOrError(f: PFResponse): DirectiveT = result.
      recover(f.andThen(failure(_))).
        getOrElse(failure(defaultErrorResponse))
  }

  object FuturableDirective {
    def successOrElse[A,R](f:Future[Option[A]], r: => ResponseFunction[R]) =
      f.map(getOrElse(_, r))
    def successOrElse[A,R](f:Option[Future[A]], r: => ResponseFunction[R]) =
      f.some(f => f.map(_.some)).none(Future{None}).map(getOrElse(_, r))
    def apply[A,B,C](future: Future[Directive[A, ResponseFunction[B], C]]) =
      new FuturableDirective[A,B,C] {
        val v = future
      }
    implicit def futureDirective[A,B,C](f: Future[Directive[A,ResponseFunction[B],C]]) =
      FuturableDirective(f)
  }

  /*
   * Try to Directive
   */
  object TriedDirective {
    def successOrElse[A,R](t: Try[A], f: => Throwable => ResponseFunction[R]) = t match {
      case scala.util.Success(a) => success(a)
      case scala.util.Failure(t: Throwable) => failure(f(t))
    }
  }
}
