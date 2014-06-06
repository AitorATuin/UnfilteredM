package com.logikujo.www

import Implicits._
import data.TriedDirective._
import plans._
import plans.blog.BlogPlan._
import model._
import model.blog.PostEntry
import model.blog.PostEntry.Implicits._
import reactivemongo.api.collections.default.BSONCollection

//import data.blog._
import AsyncDirective._

import com.github.kxbmap.configs._
import com.typesafe.config._
import unfiltered.Cycle
import unfiltered.response._
import unfiltered.request._
import unfiltered.filter.request._
import unfiltered.directives.{data => udata}
import unfiltered.directives._
import Directives._
import Result.{Success => RSuccess, Failure => RFailure}

import scala.concurrent.Future

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

  def gitHookIntent[A](dao: DAO @@ A)(config: Config[AppTest]) =
    Directive.Intent[Any, Any] {
      case ContextPath(ctx, Seg("githook" :: Nil)) => for {
        _ <- POST
        r <- unfiltered.directives.Directives.request[Any]
        _ <- contentType("application/json")
        json <- asJson(Body string r)
        addedPosts <- addedPostsFromJson(json)
        result <- addedPosts.getOrElse(List()).map(_ => )
      } yield Ok ~> ResponseString((~ addedPosts).mkString("|"))
    }

  def gitHook[A] = ##>[DAO, A, #>[AppTest, Cycle.Intent[Any, Any]]](
    (dao:DAO @@ A) => #>[AppTest, Cycle.Intent[Any, Any]](
      (config:Config[AppTest]) =>
        gitHookIntent(dao)(config).right[String]).right[String])

  val validateUser = (user:String, pass:String) => true

  import unfiltered.kit._
  def apply[A]()(implicit ev: Config[AppTest] => ErrorM[MongoDAO[A] @@ A]) = for {
    config <- configM[AppTest]
    mongo <- config.resolvM[AppTest,MongoDAO[A], A]
    hook <- gitHook[A].run(mongo).toOption.get // It's safe, it's always right
  } yield Auth.basic(validateUser)(hook)
}

trait AppTest

import reactivemongo.bson._
import reactivemongo.api._
 trait MongoDAO[A] extends DAO {
    val writer: BSONDocumentWriter[A]
    val reader: BSONDocumentReader[A]
    val col: BSONCollection

    private def insertMongo(a: A)(implicit ev: BSONDocumentWriter[A]) =
      col.insert(a: A) map (e => e.ok.
        ?(e.errMsg.getOrElse("Unknow Error").left[A]).
        |(a.right[String]))

    private def findOneMongo(query: (String, String)*)(implicit ev: BSONDocumentReader[A]) =
      col.find((BSONDocument() /: query.toList)(_ ++ _)).one[A]

    def insert(a: A) = insertMongo(a)(writer)

    def findOne(query: (String, String)*) = findOneMongo(query: _*)(reader)
  }


  object MongoDAO {
    def apply[Tag, A](conn: List[String])(db: String)(coll: String)
                     (implicit r: BSONDocumentReader[A],
                               w: BSONDocumentWriter[A]) =
      (new MongoDAO[A] {
        val reader = implicitly[BSONDocumentReader[A]]
        val writer = implicitly[BSONDocumentWriter[A]]
        val col = {
          val driver = MongoDriver()
          val con = driver.connection(conn)
          val d = con(db)
          val col = d.collection(coll).as[BSONCollection]()
          col
        }
      }).withTag[Tag]
  }
object InProduction {
  implicit val config = Configuration[AppTest]("com.logikujo.apptest")
  implicit val postsDAO =
    MongoDAO[PostEntry, PostEntry](List("localhos"))("logikujo-web")("posts")
  implicit val postsDAO2 = (c:Config[AppTest]) => {
    val connName = config.opt[List[String]]("mongo.connection").getOrElse(List("127.0.0.1"))
    val collName = c.opt[String]("blog.mongoCollection").getOrElse("posts")
    val dbName = c.opt[String]("blog.mongoDatabase").getOrElse("logikujoDB")
    MongoDAO[PostEntry, PostEntry](connName)(dbName)(collName).right[String]
  }
}

object InTest {
  implicit val config = Configuration[AppTest]("com.logikujo.apptest.test")
}

object Application  {
  import InProduction._
  def main(args: Array[String]) {
   // UnfilteredApp[AppTest]() ~> ("/" -> (GitHook() :: Nil)) run()
  }
}
