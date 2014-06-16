package com.logikujo.www

import Implicits._
import scalate._

import unfiltered.filter.async._
import unfiltered.directives._
import Directives._
import unfiltered.response._
import unfiltered.filter.request._
import scalaz._
import Scalaz._
/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.plans 17/05/14 :: 15:41 :: eof
 *
 */
package object plans {
  def RootPlan[Tag]: Tag #> Plan.Intent =
    for {
      scalate <- scalateM[Tag]
    } yield Intent {
      case req@ContextPath(ctx, "/") =>
        req.respond(Redirect("index"))
      case req@ContextPath(ctx, "/index.html") =>
        req.respond(Redirect("index"))
      case req@ContextPath(ctx, "/index") =>
        req.respond(scalate.renderString(req,"index.scaml").toOption.some(Ok ~> ResponseString(_)).none(InternalServerError))
    }

  def NotFoundPlan[Tag]: Tag #> Plan.Intent = for {
    scalate <- scalateM[Tag]
  } yield Intent {
      case req@ContextPath(ctx, path) =>
        req.respond(scalate.renderString(req,"404.scaml").toOption.some(Ok ~> ResponseString(_)).none(InternalServerError))
    }
}
