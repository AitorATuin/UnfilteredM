package com.logikujo.www
package model
package blog

import scala.util.Try
import java.io.File
import reactivemongo.bson._
import reactivemongo.api._
import reactivemongo.api.collections.default.BSONCollection
import org.joda.time.DateTime
import com.github.nscala_time.time.Imports._
import argonaut._
import Argonaut._
import laika.tree._
import laika.parse.markdown._
import laika.render._
import laika.api.{Parse, Render}
import com.typesafe.config.Config
import com.github.kxbmap.configs._
import scalaz._
import Scalaz._
import scala.concurrent.ExecutionContext.Implicits.global
import org.slf4j.LoggerFactory
import scala.concurrent.{Future, Await}
import scala.concurrent.duration._
import scala.concurrent.duration.{Duration => Dur}

sealed case class PostEntry(title: String, contents: String, publishDate: DateTime) {
  lazy val id = title.trim.replace(" ", "_").toLowerCase

  def withId(newTitle: String): PostEntry = new PostEntry(title, contents, publishDate) {
    override lazy val id = newTitle.trim.replace(" ", "_").toLowerCase
  }
}

trait Post[A] {
  def contents(post:A): String
}
trait PostOps[A] {
  val v: A
  def contents(implicit ev:Post[A]) = ev.contents(v)
}

object PostEntry {
    import Implicits._ // model implicits
    trait HeadersMarkup {
      //val headers: List[(String, String) => Validation]
      val doc: Documents.Document

      private def get(s: String): ValidationNel[String, String] =
        Try(doc.config.getString(s)).toOption.
          some {
          _.successNel[String]
        }.
          none {
          s"""Couldnt get field $s""".failNel[String]
        }

      //lazy val hMap =  headers.map { a => get(a) }.toMap
      def apply(s: String): ValidationNel[String, String] = get(s)

      //def apply2[A](s:String): ValidationNel[String, String] = (h:HeadersMarkup => ValidationNel)
    }

    object HeadersMarkup {
      def apply(d: Documents.Document) = new HeadersMarkup {
        //val headers = h
        val doc = d
      }
    }

    private val markdownParser = Parse as Markdown

    private def getTitle(m: HeadersMarkup)(doc: Documents.Document) =
      m("title") ||| {
        val t = doc.title.map(Render as HTML from _ toString)
        if (t.isEmpty) "Couldnt get title".failureNel[String] else (t mkString).successNel[String]
      }

    private def getDate(m: HeadersMarkup)(doc: Documents.Document) =
      m("date").flatMap {
        s => s.toDateTimeOption.toSuccess("Couldnt parse date field").toValidationNel
      } ||| (DateTime.now.successNel[String])

    def fromString(str: String) = for {
      doc <- \/-(markdownParser fromString str)
      header <- \/-(HeadersMarkup(doc))
      title <- getTitle(header)(doc).disjunction
      date <- getDate(header)(doc).disjunction
      contents <- \/-(Render as HTML from doc.content toString)
    } yield PostEntry(title, contents, date)

    def fromFile(file: String) = {
      val getDoc = (file: String) => {
        val d = for {
          f <- Some(new File(file))
          doc <- (Parse as Markdown fromFile (f.toString)).some
          if f.isFile && f.canRead
        } yield doc
        d.\/>( s"""Unable to open or read file $file""".wrapNel)
      }
      for {
        doc <- getDoc(file)
        header <- \/-(HeadersMarkup(doc))
        title <- getTitle(header)(doc).disjunction
        date <- getDate(header)(doc).disjunction
        contents <- \/-(Render as HTML from doc.content toString)
      } yield PostEntry(title, contents, date)
    }

  object Implicits {
    import model.Implicits._
    // PostEntry BSON Writer
    implicit object PostEntryWriter extends BSONDocumentWriter[PostEntry] {
      def write(post: PostEntry): BSONDocument = BSONDocument(
        "title" -> post.title,
        "id" -> post.id,
        "contents" -> post.contents,
        "publishDate" -> post.publishDate
      )
    }

    // PostEntry BSON Reader
    implicit object PostEntryReader extends BSONDocumentReader[PostEntry] {
      def read(post: BSONDocument): PostEntry = PostEntry(
        post.getAs[String]("title").get,
        post.getAs[String]("contents").get,
        post.getAs[DateTime]("publishDate").get
      )
    }

    // PostEntry Json Encoder
    implicit val PostEntryEncodeJson: EncodeJson[PostEntry] =
      EncodeJson((p: PostEntry) =>
        ("title" := p.title) ->:
          ("id" := p.id) ->:
          ("contents" := p.contents) ->:
          ("publishDate" := p.publishDate) ->: jEmptyObject)

    // PostEntry Json Decoder
    implicit val PostEntryDecodeJson: DecodeJson[PostEntry] =
      DecodeJson(c => for {
        title <- (c --\ "title").as[String]
        contents <- (c --\ "contents").as[String]
        publishDate <- (c --\ "publishDate").as[DateTime]
        id <- (c --\ "id").as[String]
      } yield PostEntry(title, contents, publishDate).withId(id))

    implicit object PostEntryAsRestRecord extends RestRecord[PostEntry] {
      def id(post: PostEntry) = post.id

      def onCreate(post: PostEntry) =
        ("title" := post.title) ->: ("id" := post.id) ->: jEmptyObject
    }

    implicit object PostEntryAsMongoRecord extends MongoRecord[PostEntry] {
      def col: Configuration => Mongo => BSONCollection = c => mongo => {
        val collectionName = c.opt[String]("blog.mongoCollection").getOrElse("posts")
        val db = mongo.conn("logikujo-web")
        db.collection(collectionName).as[BSONCollection]()
      }
    }

    implicit object PostEntryAsPost extends Post[PostEntry] {
      def contents(post: PostEntry) = post.contents
    }

    implicit def toPostOps[A:Post](a:A) = new PostOps[A] {
      val v = a;
    }
  }
}
