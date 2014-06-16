package com.logikujo.www

import Implicits._
import data.TriedDirective._
import plans._
import plans.blog._
import model._
import model.blog.PostEntry
import model.blog.PostEntry.Implicits._
import mail._

import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.core.commands.LastError
import reactivemongo.api.indexes.{Index,IndexType}

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
trait Contact

object InProduction {
  def getCol(config: Config[AppTest]) = {
    val connName = config.opt[List[String]]("mongo.connection").getOrElse(List("127.0.0.1"))
    val collName = config.opt[String]("blog.mongo.collection").getOrElse("posts")
    val dbName = config.opt[String]("blog.mongo.database").getOrElse("logikujoDB")
    (connName, dbName, collName)
  }

  // Main config.
  implicit val config = Configuration[AppTest]("com.logikujo.apptest")

  // DAO for post entries
  implicit val postDAO = (c:Config[AppTest]) => (getCol(c) match {
    case (con, col, db) =>
      MongoDBDAO[PostEntry](con)(col)(db).
        withIndex[PostEntry](Index(key = Seq("id" -> IndexType.Ascending), unique = true))
  }).right[String]

  // Mail to send notifications after blog updates
  implicit val mailBlogNotification = (c:Config[AppTest]) => (for {
    to <- c.opt[String]("blog.mail.notification")
    cc <- c.opt[List[String]]("blog.mail.notificationCC")
    bcc <- c.opt[List[String]]("blog.mail.notificationBCC")
  } yield Mail().to(to).cc(cc).bcc(bcc)).\/>("Mail blog notification not configured")

  // Mail to send notifications about contact requests
  implicit val mailContact: AppTest ?> (Mail @@ Contact) = (c:Config[AppTest]) => (for {
    to <- c.opt[String]("contact.contactAddress")
    cc <- c.opt[List[String]]("contact.contactCC")
    bcc <- c.opt[List[String]]("contact.contactBCC")
  } yield Mail().to(to).cc(cc).bcc(bcc).withTag[Contact]).\/>("Mail contact not configured.")

  // Mail subsystem used
  implicit val mailSystem: AppTest ?> (MailSender @@ AppTest) =
    (c:Config[AppTest]) =>
      c.atPath[AppTest]("myMailSystem") map (MailSender[AppTest](_))
}

object InTest {
  //implicit val config = Configuration[AppTest]("com.logikujo.apptest.test")
}

object BlogPlan2 {
  type %>[App, A] = Config[App] => ErrorM[A]
  import reactivemongo.bson.BSONDocumentReader
  import scalate._
  import scala.util.{Success => SSuccess, Failure => SFailure}
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
          AsyncResponse(postEntry) {
            case SSuccess(post) =>
              // Todo: renderString to response should be done in scalate with a function
              post.
                ?(scalate.renderString(request, "blogPost.scaml", "post" -> post.get.toString) match {
                case SSuccess(page) => Ok ~> ResponseString(page)
                case SFailure(t) => InternalServerError ~> ResponseString("Not Implemented")
              }).
                | {
                NotFound ~> Redirect("/blog/404")
              }
            case SFailure(t) => InternalServerError ~> ResponseString("Not Implemented")
          }
        }
    }
}

object Application  {
  import InProduction._
  object AsyncPlan3 {
    def intent = unfiltered.filter.async.Intent {
      case ContextPath(ctx, Seg("uno" :: Nil)) => Pass
      case req@ContextPath(ctx, Seg("async" :: Nil)) =>
        req.respond(ResponseString("test") ~> Ok)
      case req@ContextPath(ctx, Seg("async1" :: Nil)) =>
        //"respond" is usually called from an asynchronous callback handler
        req.respond(ResponseString("test1") ~> Ok)

        //"respond" is usually called from an asy
    }
    def intentM[T] = for {
      config <- configM[T]
    } yield intent
  }
  object AsyncPlan4   {
    def intent = unfiltered.filter.async.Intent{
      case ContextPath(ctx,Seg("pass2" :: Nil)) => Pass
      case req@ContextPath(ctx, Seg("async2" :: Nil)) =>
        //"respond" is usually called from an asynchronous callback handler
        req.respond(ResponseString("test2") ~> Ok)
    }
    def intentM[T] = for {
      config <- configM[T]
    } yield intent
  }

  import scalate._
  def NotFoundPlan2[Tag] = for {
    scalate <- scalateM[Tag]
  } yield unfiltered.filter.async.Intent {
      case req@ContextPath(ctx, path) =>
        req.respond(scalate.renderString(req,"404.scaml").toOption.some(Ok ~> ResponseString(_)).none(InternalServerError))
    }
  /*def main(args: Array[String]) {
    println("STARTING")
    unfiltered.jetty.Http.local(8080).
      filter(unfiltered.filter.async.Planify(AsyncPlan3.intent orElse AsyncPlan4.intent)).run()
    //unfiltered.netty.Http.local(8080).plan(hello).run()
  }*/
  def main(args: Array[String]) {
    //val t1 = AsyncPlan3.intentM[AppTest]
    //val t2 = AsyncPlan4.intentM[AppTest]
    //val o = (t1 |@| t2 |@| RootPlan[AppTest] |@| BlogPlan2[AppTest, PostEntry] |@| NotFoundPlan[AppTest]) {_ orElse _ orElse _ orElse _ orElse _}
    //println(o)
    UnfilteredApp[AppTest]() ~>
      ("/" -> (
        AsyncPlan3.intentM[AppTest] ::
          AsyncPlan4.intentM[AppTest] ::
          RootPlan[AppTest] ::
          BlogPlan2[AppTest, PostEntry] ::
          NotFoundPlan[AppTest] ::
          Nil)) run()
       //("/hook" -> (GitHook[PostEntry]().as[async.Plan] :: Nil)) run()
  }
}
