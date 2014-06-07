package com.logikujo.www
package model

import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson._
import reactivemongo.api._
import reactivemongo.api.collections.default.BSONCollection
import scala.util.Try
import com.typesafe.config.Config
import com.github.kxbmap.configs._
import scala.concurrent.ExecutionContext.Implicits.global
import scalaz._
import Scalaz._
import org.slf4j.LoggerFactory
import scala.concurrent.Future
import reactivemongo.core.commands.LastError

/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.model 29/05/14 :: 20:19 :: eof
 *
 */
trait MongoRecord[A] {
  val logger = LoggerFactory.getLogger(classOf[MongoRecord[A]])

  def col: Configuration => Mongo => BSONCollection

  def findOne(query: BSONDocument)(mongo: Mongo)(implicit ev: BSONDocumentReader[A]) =
    liftM((c: Configuration) => col(c)(mongo).find(query).one[A].right[String])

  def insert(a: A)(mongo: Mongo)(implicit ev: BSONDocumentWriter[A]) =
    liftM(col(_)(mongo).insert(a).right[String])
}

// TODO: Remove
sealed trait Mongo {
  val logger = LoggerFactory.getLogger(classOf[Mongo])

  val config: Configuration
  lazy val driver = MongoDriver()
  lazy val conn = driver.connection(config.opt[List[String]]("mongo.connection").getOrElse(List("127.0.0.1")))
}

trait MongoDBDAO extends DAO {
  val col: BSONCollection

  def insert[A](a: A)(implicit ev: BSONDocumentWriter[A]) =
    col.insert(a: A) map (e => e.ok.
      ?(e.errMsg.getOrElse("Unknow Error").left[A]).
      |(a.right[String]))

  def findOne[A](query: (String, String)*)(implicit ev: BSONDocumentReader[A]) =
    col.find((BSONDocument() /: query.toList)(_ ++ _)).one[A]
}

object MongoDBDAO {
  def apply[Tag](conn: List[String])(db: String)(coll: String) =
    (new MongoDBDAO {
      val col = {
        val driver = MongoDriver()
        val con = driver.connection(conn)
        val d = con(db)
        val col = d.collection(coll).as[BSONCollection]()
        col
      }
    }).withTag[Tag]
}

object Mongo  {
  val logger = LoggerFactory.getLogger(classOf[Mongo])
  type UnfilteredMongoM = UnfilteredM[Mongo]
  def unfilteredMongoM = {
    liftM[Mongo]((c:Configuration) => {
      (new Mongo {
        val config = c
      }).right[String]
    })
  }
}

