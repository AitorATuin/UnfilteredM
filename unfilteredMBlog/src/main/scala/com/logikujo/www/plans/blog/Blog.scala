package com.logikujo.www
package plans
package blog

import model.MongoDBDAO
import AsyncDirective.AsyncResponse
import scalate._

import unfiltered.filter.async
import unfiltered.request._
import unfiltered.filter.request._
import unfiltered.response._
import unfiltered.directives._
import unfiltered.directives.Directives._

import scala.util.{Success => SSuccess, Failure => SFailure}
import scala.concurrent.Future

import reactivemongo.bson.{BSONDocument, BSONDocumentReader}

import argonaut._
import Argonaut._

import scalaz._
import Scalaz._

import org.slf4j.LoggerFactory

/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.plans 9/01/14 :: 20:31 :: eof
 *
 */

trait BlogParameters {
  def ff(title: String): Future[Map[String, Any]]
  def apply(title: String) = ff(title)
}
object BlogParameters {
  def apply[Tag](f:String => Future[Map[String, Any]]) = new BlogParameters {
    def ff(title: String) = f(title)
  }.withTag[Tag]
}

// TODO: Make PostEntry to be able to take data from any store
protected trait BlogIntents {
  val logger = LoggerFactory.getLogger(classOf[BlogIntents])

  /*val restAPIPath = List("api", "v1", "post")
  val blogPath = List("blog")
  val logger = LoggerFactory.getLogger(classOf[BlogIntents])

  object ConfiguredSeg {

    sealed trait ConfiguredSegImpl {
      val configPath: List[String]

      def unapply(path: String): Option[List[String]] = path.split("/").toList match {
        case "" :: rest if rest.containsSlice(configPath) => Some(rest.drop(configPath.length))
        case all if all.containsSlice(configPath) => Some(all.drop(configPath.length))
        case _ => None
      }
    }

    def apply(path: List[String]) = new ConfiguredSegImpl {
      val configPath = path
    }

    def apply(cfgPath: String, defaultPath: List[String]): UnfilteredM[ConfiguredSeg.ConfiguredSegImpl] =
      liftM((c: Configuration) =>
        apply(c.opt[List[String]](cfgPath).getOrElse(defaultPath)).right[String])
  }*/
}
// TODO: add getOrElse for disjunctions and replace in config.opt with config.disjunction
// TODO: add response in case of development mode if blogTmpl is not defined
object BlogPlan extends BlogIntents {
  def apply[App, Post: BSONDocumentReader](path: String = "/")
  (implicit a: App ?> @@[MongoDBDAO, Post],
            b: App ?> @@[BlogParameters, Post]) =
    for {
      config <- configM[App].flatMapK(_.atPath(path))
      dao <- config.resolvM[App, MongoDBDAO, Post]
      scalate <- scalateM[App]
      scalateParams <- config.resolvM[App, BlogParameters, Post]
    } yield AsyncDirective[Any, Map[String, Any]] {
      case ContextPath(ctx, Seg("post" :: title :: Nil)) => for {
        _ <- GET
        request <- Directives.request[Any]
        blogTmpl <- getOrElse(config.opt[String]("blog.postTemplate"),
          InternalServerError ~> ResponseString("blog.postTemplate"))
        //postEntry <- success(dao.findOne[Post](BSONDocument("id" -> title.toLowerCase)))
        params <- success(scalateParams(title))
      } yield {
        AsyncResponse(params) {
          case SSuccess(post) =>
            scalate(request, blogTmpl, post.toList:_*)
          case SFailure(t) => InternalServerError ~> ResponseString("E500 :: InternalServerError")
        }
      }
    }
}
