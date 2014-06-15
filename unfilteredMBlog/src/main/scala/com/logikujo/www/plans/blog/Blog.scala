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

import reactivemongo.bson.BSONDocumentReader

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
object BlogPlan extends BlogIntents {
  type %>[App, A] = Config[App] => ErrorM[A]

  def apply[App, Post : BSONDocumentReader](implicit ev: App %> @@[MongoDBDAO, Post]) =
    for {
      config <- configM[App]
      dao <- config.resolvM[App, MongoDBDAO, Post]
      scalate <- scalateM[App]
    } yield AsyncDirective[Any, Option[Post]] {
        case ContextPath(ctx, Seg("post" :: title :: Nil)) => for {
          _ <- GET
          request <- Directives.request[Any]
          postEntry <- success(dao.findOne[Post]("id" -> title.toLowerCase))
        } yield {
          logger.debug("KKKKKKKK")
          AsyncResponse(postEntry) {
            case SSuccess(post) =>
              logger.debug("Aqui estamos")
              // Todo: renderString to response should be done in scalate with a function
              post.
                ?(scalate.renderString(request, "blogPost.scaml", "post" -> post.get.toString) match {
                case SSuccess(page) => Ok ~> ResponseString(page)
                case SFailure(t) => InternalServerError ~> ResponseString("Not Implemented")
              }).
                | {
                logger.debug("No hay postss!")
                NotFound ~> Redirect("/blog/404")
              }
            case SFailure(t) => InternalServerError ~> ResponseString("Not Implemented")
          }
        }
    }
}
