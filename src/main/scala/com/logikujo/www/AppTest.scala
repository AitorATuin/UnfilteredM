package com.logikujo.www

import Implicits._
import data.TriedDirective._
import plans._
import plans.blog._
import model._
import model.blog.PostEntry
import model.blog.PostEntry.Implicits._
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.core.commands.LastError

//import data.blog._
import AsyncDirective._

import com.github.kxbmap.configs._
import com.typesafe.config._
import unfiltered.Cycle
import unfiltered.response._
import unfiltered.request._
import unfiltered.filter._
import unfiltered.filter.request._
import unfiltered.directives.{data => udata}
import unfiltered.directives._
import Directives._
import Result.{Success => RSuccess, Failure => RFailure}

import scala.concurrent.Future
import scala.util.{Success => TSuccess, Failure => TFailure, Try}

import argonaut._
import Argonaut._
import scalaz._
import Scalaz._

import dispatch._
import org.slf4j.LoggerFactory
import scala.concurrent.ExecutionContext.Implicits.global

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
    when { case _ if json.isRight => json.toOption.get} orElse BadRequest ~> ResponseString("Bad json")
  }

  // TODO: dev mode responses?
  def parseJson[A](v:Option[A]) =
    when { case _ if v.isDefined => v } orElse BadRequest ~> ResponseString("Bad json")

  def addedPostsFromJson(json: Json) = parseJson {
    for {
      commits         <- json -| "commits"
      commitsArray    <- commits.array
      addedItems      <- commitsArray.map(_.fieldOrEmptyArray("added")).some
      addedItemsArray <- addedItems.map(_.arrayOrEmpty).some
    } yield addedItemsArray.flatten.map(_.stringOr(""))
  }

  def getPostsFromUrl[A](config: Config[A])(urls: List[String]) = {
    type ListsResult = (List[Future[PostEntry]], List[String])
    val url = for {
      repoBase <- config.opt[String]("blog.repoBase")
      urlBase <- config.opt[String]("blog.urlBase")
    } yield urlBase + "/" + repoBase
    url.
      some((url: String) => {
        val (ss,fs) = urls.map(p => PostEntry.fromURL(s"${url}/${p}").leftMap(p + "::" + _)) partition {
          case \/-(f) => true
          case _ => false
        }
        result[ResponseFunction[Any], ListsResult](RSuccess((ss.map(_.toOption.get), fs.map(_.swap.toOption.get))))
      }).
      none(result[ResponseFunction[Any], ListsResult](RFailure(NotImplemented ~> ResponseString("Service not available"))))
  }

  def mySuccess[A](v: \/[String,A]) = v match {
    case \/-(a) => success(a)
    case -\/(e) => failure(BadRequest ~> ResponseString(e))
  }

  def futureToFutureTry[A](f: Future[A]): Future[Try[A]] =
    f.map(TSuccess(_)).recover { case x => TFailure(x) }

  def gitHookIntent[A](dao: MongoDBDAO @@ A)(config: Config[AppTest]) =
    AsyncDirective[Any, List[Try[LastError]]] {
      case ContextPath(ctx, Seg("blog" :: Nil)) => for {
        _                   <- POST
        r                   <- unfiltered.directives.Directives.request[Any]
        _                   <- contentType("application/json")
        json                <- asJson(Body string r)
        addedPosts          <- addedPostsFromJson(json)
        (parsedS, parsedF)  <- getPostsFromUrl(config)(addedPosts.getOrElse(List()))
        result              <- success(parsedS.map(for {
                              p <- _
                              r <- dao.insert(p)
                            } yield r).map(f => futureToFutureTry(f)))
        resultAsFuture <- success(Future.sequence(result))
      } yield AsyncResponse(resultAsFuture) {
          case TSuccess(l) =>
            val (insertedS, insertedF) = l partition {
              case TSuccess(_) => true
              case _ => false
            }
            logger.debug("Errors:" + (insertedF ++ parsedF).toString)
            logger.debug(insertedS.toString)
            Ok ~> ResponseString(s"Success: ${insertedS.length} posts; Error: ${(insertedF ++ parsedF).length} posts")
          case TFailure(e) => BadRequest ~> ResponseString("Unexpected error") // should never reach this point
      }
    }

  def gitHook[A] = ##>[MongoDBDAO, A, #>[AppTest, async.Plan.Intent]](
    (dao:MongoDBDAO @@ A) => #>[AppTest, async.Plan.Intent](
      (config:Config[AppTest]) =>
        gitHookIntent(dao)(config).right[String]).right[String])

  val validateUser = #>[AppTest, (String, String) => Boolean]((c: Config[AppTest]) =>
    ((user:String, pass:String) => {
      if (user.some == c.opt[String]("blog.gitHookUser") && pass.some == c.opt[String]("blog.gitHookPass")) true
      else false
    }).right[String])

  def apply[A]()(implicit ev: Config[AppTest] => ErrorM[MongoDBDAO @@ A]) = for {
    config <- configM[AppTest]
    mongo <- config.resolvM[AppTest,MongoDBDAO, A]
    hook <- gitHook[A].run(mongo).toOption.get // It's safe, it's always right
    validate <- validateUser
  } yield Auth.basic(validate)(hook)
  //yield Auth.basic((user:String, pass:String) => true)(hook)
}

trait AppTest

object InProduction {
  def getCol(config: Config[AppTest]) = {
    val connName = config.opt[List[String]]("mongo.connection").getOrElse(List("127.0.0.1"))
    val collName = config.opt[String]("blog.mongo.collection").getOrElse("posts")
    val dbName = config.opt[String]("blog.mongo.database").getOrElse("logikujoDB")
    (connName, dbName, collName)
  }
  implicit val config = Configuration[AppTest]("com.logikujo.apptest")
  implicit val postDAO = (c:Config[AppTest]) => (getCol(c) match {
    case (con, col, db) => MongoDBDAO[PostEntry](con)(col)(db)
  }).right[String]
}

object InTest {
  //implicit val config = Configuration[AppTest]("com.logikujo.apptest.test")
}

object Application  {
  import InProduction._
  def main(args: Array[String]) {
     UnfilteredApp[AppTest]() ~>
       ("/" -> (
                BlogPlan[AppTest, PostEntry].as[async.Plan] ::
                RootPlan[AppTest] ::
                NotFoundPlan[AppTest] ::
                Nil)) ~>
       ("/hook" -> (GitHook[PostEntry]().as[async.Plan] :: Nil)) run()
  }
}
