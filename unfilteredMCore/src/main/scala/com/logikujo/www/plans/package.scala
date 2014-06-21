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
import org.slf4j.LoggerFactory

/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.plans 17/05/14 :: 15:41 :: eof
 *
 * TODO: Add support for disjunction in indexTpml
 * TODO: Add support for development flag in indelTmpl
 * TODO: Refactor the sequence map orElse foreach in both plans
 *
 */
package object plans {
  val logger = LoggerFactory.getLogger("unfilteredM.plans")
  def RootPlan[App](path: String = "/"): App #> Plan.Intent =
    for {
      config <- configM[App].flatMapK(_.atPath(path))
      scalate <- scalateM[App]
    } yield Intent {
      case req@ContextPath(ctx, "/") =>
        req.respond(Redirect("index"))
      case req@ContextPath(ctx, "/index.html") =>
        req.respond(Redirect("index"))
      case req@ContextPath(ctx, "/index") => (for {
          indexTmpl <- config.opt[String]("indexTemplate")
        } yield indexTmpl).
          map(scalate(req,_)).
          orElse(Some(InternalServerError ~> ResponseString("indexTemplate"))).
          foreach(req.respond(_))
    }

  def NotFoundPlan[App](path: String = "/"): App #> Plan.Intent = for {
    config <- configM[App].flatMapK(_.atPath(path))
    scalate <- scalateM[App]
  } yield Intent {
      case req@ContextPath(ctx, path) => (for {
        notFoundTemplate <- config.opt[String]("404Template")
      } yield notFoundTemplate).
        map(scalate(req,_)).
        orElse(Some(InternalServerError ~> ResponseString("404Template"))).
        foreach(req.respond(_))
    }
}
