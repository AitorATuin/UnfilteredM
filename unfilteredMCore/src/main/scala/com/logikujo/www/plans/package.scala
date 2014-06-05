package com.logikujo.www

import Implicits._
import scalate._

import unfiltered.filter.Plan
import unfiltered.directives._
import Directives._
import unfiltered.response._
import unfiltered.filter.request._
/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.plans 17/05/14 :: 15:41 :: eof
 *
 */
package object plans {
  def RootPlan[Tag]: Tag #> Plan =  {
    val a = for {
      scalate <- scalateM[Tag]
    } yield Directive.Intent[Any,Any] {
      case ContextPath(ctx, "/") => success(Redirect("index"))
      case ContextPath(ctx, "/index.html") => success(Redirect("index"))
      case ContextPath(ctx, "/index") => scalate("index.scaml")
    }
    a.as[Plan]
  }

  def NotFoundPlan[Tag]: Tag #> Plan = (for {
    scalate <- scalateM[Tag]
  } yield Directive.Intent[Any, Any] {
      case ContextPath(ctx, path) =>
        scalate.render("404.scaml").map(NotFound ~> ResponseString(_))
    }).as[Plan]
}
