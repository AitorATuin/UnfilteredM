package com.logikujo.www

import Implicits._
import data.TriedDirective._
import plans._
import plans.blog.BlogPlan._
import model.blog.PostEntry
import model.blog.PostEntry.Implicits._
//import data.blog._
import AsyncDirective._

import com.github.kxbmap.configs._
import com.typesafe.config._
import unfiltered.response._
import unfiltered.request._
import unfiltered.filter.request._
import unfiltered.directives.{data => udata}
import unfiltered.directives._
import Directives._
import Result.{Success => RSuccess, Failure => RFailure}
import argonaut._
import Argonaut._
import scalaz._
import Scalaz._

import dispatch._
import org.slf4j.LoggerFactory


/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www 10/05/14 :: 17:58 :: eof
 *
 */
 // Http.configure(_ setFollowRedirects true)(url(s) OK as.String)}
object GitHook {
  trait Users {
    def auth(u: String, p: String): Boolean
  }

  val logger = LoggerFactory.getLogger(classOf[AppTest])
  private val githubUrl = "https://github.com/login/oauth/access_token"

  def contentType(tpe: String) =
    when { case RequestContentType(`tpe`) =>} orElse UnsupportedMediaType

  // Parse a json
  def asJson(str: String) = {
    val json = Parse.parse(str)
    when { case _ if json.isRight => json.toOption.get} orElse BadRequest
  }

  def parseJson[A](v:Option[A]) =
    when { case _ if v.isDefined => v } orElse BadRequest

  def addedPostsFromJson(json: Json) = parseJson {
    for {
      commits         <- json -| "commits"
      commitsArray    <- commits.array
      addedItems      <- commitsArray.map(_.fieldOrEmptyArray("added")).some
      addedItemsArray <- addedItems.map(_.arrayOrEmpty).some
    } yield addedItemsArray.flatten.map(_.toString)
  }

  def gitHookIntent = Directive.Intent[Any, Any] {
    case ContextPath(ctx, Seg("githook" :: Nil)) => for {
      _ <- POST
      r <- unfiltered.directives.Directives.request[Any]
      _ <- contentType("application/json")
      json <- asJson(Body string r)
      addedPosts <- addedPostsFromJson(json)
    } yield Ok ~> ResponseString((~ addedPosts).mkString("|"))
  }
  val validateUser = (user:String, pass:String) => true

  import com.logikujo.www._
  import com.logikujo.www.model.Mongo._
  import unfiltered.kit._
  def apply()  = for {
    config <- configM
    //mongo <- unfilteredMongoM
    //gitHook <- kit.Auth.basic(validateUser)(gitHookIntent)
  } yield Auth.basic(validateUser)(gitHookIntent)
}

trait AppTest

object InProduction {
  implicit val config = Configuration[AppTest]("com.logikujo.apptest")
}

object InTest {
  implicit val config = Configuration[AppTest]("com.logikujo.apptest.test")
}

object Application  {
  import InProduction._
  def main(args: Array[String]) {
    UnfilteredApp[AppTest]() ~> ("/" -> (GitHook() :: Nil)) run()
  }
}
