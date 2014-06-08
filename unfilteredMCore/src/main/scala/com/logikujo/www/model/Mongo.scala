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

trait MongoDBDAO extends DAO {
  val col: BSONCollection

  def insert[A](a: A)(implicit ev: BSONDocumentWriter[A]) =
    col.insert(a: A)

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

