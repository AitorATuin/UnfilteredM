package com.logikujo.www
package model

import reactivemongo.api.indexes.{Index, IndexType}
import reactivemongo.bson._
import reactivemongo.api._
import reactivemongo.api.collections.default.BSONCollection
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}
import org.slf4j.LoggerFactory


/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.model 29/05/14 :: 20:19 :: eof
 *
 * TODO: Add debug and error messages
 *
 */

trait MongoDBDAO extends DAO {
  val logger = LoggerFactory.getLogger(classOf[MongoDBDAO])

  val col: BSONCollection

  def insert[A](a: A)(implicit ev: BSONDocumentWriter[A]) =
    col.insert(a: A)

  def findOne[A](query: (String, String)*)(implicit ev: BSONDocumentReader[A]) =
    col.find((BSONDocument() /: query.toList)(_ ++ _)).one[A]

  def withIndex[Tag](index: Index) = {
    val indexesString = index.key.map(_._1).mkString(":")
    col.indexesManager.ensure(index).onComplete {
      case Success(b) =>
        if (b) logger.debug("Created indexes: " + indexesString)
        else logger.error(s"Index yet created: ${col.indexesManager.collectionName} -- ${indexesString}")
      case Failure(t) =>
        logger.error(s"Couldnt create index: " + indexesString + ". Reason: " + t.toString)
    }
    this.withTag[Tag]
  }
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

