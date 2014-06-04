package com.logikujo.www
package plans
package blog

import scalate._
import data._
import data.blog._
import model._
import model.Mongo._
import model.blog.PostEntry._
import error._
import error.blog._
import com.logikujo.www.Implicits._
import com.logikujo.www.model.blog.PostEntry.Implicits._

import argonaut._
import Argonaut._
import unfiltered.directives._
import Directives._
import unfiltered.filter._
import unfiltered.filter.request._
import unfiltered.response._
import unfiltered.directives.data.Interpreter
import unfiltered.request._
import scalaz._
import Scalaz._
import scala.util.{Success, Failure}
import reactivemongo.bson.{BSONDocument, BSONDocumentReader, BSONDocumentWriter}
import reactivemongo.core.errors.DatabaseException
import scala.concurrent.ExecutionContext.Implicits.global
import com.typesafe.config.{ConfigFactory, Config}
import com.github.kxbmap.configs._
import scala.concurrent.Future
import reactivemongo.core.commands.LastError

import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.util.Try
import org.slf4j.LoggerFactory
import com.logikujo.www.model.blog.Post
//import unfiltered.filter.async.Plan
/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.plans 9/01/14 :: 20:31 :: eof
 *
 */

// TODO: Make PostEntry to be able to take data from any store
protected trait BlogIntents {
  val restAPIPath = List("api", "v1", "post")
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
  }

  import FuturableDirective._
  import ConfiguredSeg._

  def postEntry[A](mongo: Mongo)(title: String)(implicit mongoRecord: MongoRecord[A], ev: BSONDocumentReader[A]) = {
    (for {
      result <- mongoRecord.findOne(BSONDocument("id" -> title.toLowerCase))(mongo)
    } yield result).flatMapK {
      f => Try(Await.result(f, 500 millis)).map(\/-(_)).getOrElse(-\/("Error"))
    }.mapK[ResultM, Option[A]] {
      case \/-(s) => Result.Success(s)
      case -\/(e) => Result.Failure(BlogPostNotFound)
    }.flatMapK {
      case Some(a) => Result.Success(a)
      case None => Result.Failure(BlogPostNotFound)
    }
  }

  def insertPost[A](mongo: Mongo)(p: A)(implicit mongoRecord: MongoRecord[A], ev: BSONDocumentWriter[A]) = {
    type ResultT[+A] = \/[Throwable, A]
    (for {
      result <- mongoRecord.insert(p)(mongo)
    } yield result).
      mapK[ResultT, Future[LastError]](_.leftMap(new Exception(_))). // Map the inside monad to get a ResultT
      flatMapK(f => Try(Await.result(f, 1000 millis)) match {
      // flatMap inside the monad
      case scala.util.Success(s) => \/-(s)
      case scala.util.Failure(e) => -\/(e)
    }).
      mapK[ResultM, LastError] {
      // Map again the monad to get a ResultM again
      case \/-(s) => Result.Success(s)
      case -\/(e) => e match {
        case e: DatabaseException if e.code == 11000.some => Result.Failure(BlogPostConflict)
        case e: Throwable => Result.Failure(BlogBadRequest(("post" := "Error inserting post") ->: jEmptyObject))
      }
    }
  }

  def restIntent[A](config: Configuration, mongo: Mongo, PostSeg: ConfiguredSegImpl)(implicit
                                                                              mongoRecord: MongoRecord[A],
                                                                              r: BSONDocumentReader[A],
                                                                              w: BSONDocumentWriter[A],
                                                                              e: EncodeJson[A],
                                                                              d: DecodeJson[A],
                                                                              i: Interpreter[Seq[String], Option[A], ServerResponse[Any]],
                                                                              rRec: RestRecord[A]) = {
    //val PostSeg = ConfiguredSeg(config.opt[List[String]]("blog.restPath").
    //  getOrElse("api" :: "v1" :: "posts" :: Nil))
    Directive.Intent[Any, Any] {
      case Path(PostSeg(title :: Nil)) => for {
        _ <- GET
        _ <- Accepts.Json
        request <- Directives.request[Any]
        postEntry <- Directive((_: HttpRequest[Any]) => postEntry[A](mongo)(title).run(config))
      } yield BlogSuccess(postEntry.asJson)
      // POST api/v1/posts
      case Path(PostSeg(Nil)) => for {
        _ <- POST
        _ <- Accepts.Json
        post <- data.as.Required[A] named "url"
        //status <- savePostEntry(post.some) runOrError {
        status <- Directive((_: HttpRequest[Any]) => insertPost(mongo)(post).run(config)) /*runOrError {
          case e: DatabaseException if e.code == 11000.some => BlogPostConflict
        }*/
      } yield {
        BlogPostCreated(post)
      }
    }
  }

  def blogIntent[A](scalate: Scalate, config: Configuration, mongo: Mongo, PostSeg: ConfiguredSegImpl)
                   (implicit
                    mongoRecord: MongoRecord[A],
                    r: BSONDocumentReader[A],
                    w: BSONDocumentWriter[A],
                    e: EncodeJson[A],
                    d: DecodeJson[A],
                    i: Interpreter[Seq[String], Option[A], ServerResponse[Any]],
                    rRec: RestRecord[A],
                    p: Post[A]) =
    Directive.Intent[Any, Any] {
      //case ContextPath(ctx, PostSeg(Nil)) => scalate("blogPost.scaml")
      case ContextPath(ctx, PostSeg(title :: Nil)) => (for {
        _ <- GET
        request <- Directives.request[Any]
        postEntry <- Directive((_: HttpRequest[Any]) => postEntry[A](mongo)(title).run(config))
      } yield postEntry).
        flatMap(post => scalate("blogPost.scaml", "post" -> post.contents)).
        orElse(failure(NotFound ~> Redirect("/blog/404")))
      //case ContextPath(ctx, PostSeg(xs)) => failure(NotFound ~> Redirect("404"))
    }
}

object BlogPlan extends BlogIntents {
  def blogPlan[A]()(implicit
                    mongoRecord: MongoRecord[A],
                    r: BSONDocumentReader[A],
                    w: BSONDocumentWriter[A],
                    e: EncodeJson[A],
                    d: DecodeJson[A],
                    i: Interpreter[Seq[String], Option[A], ServerResponse[Any]],
                    rRec: RestRecord[A],
                    post: Post[A]) = {
    for {
      mongo <- unfilteredMongoM
      scalate <- scalateM
      config <- configM
      blogAPIPath <- ConfiguredSeg("blog.restPath", restAPIPath)
      blogPath <- ConfiguredSeg("blog.blogPath", blogPath)
      blogRest <- restIntent[A](config, mongo, blogAPIPath)
      blog <- blogIntent[A](scalate, config, mongo, blogPath)
    } yield blog.orElse(blogRest)
  }
}
