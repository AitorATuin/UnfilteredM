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

  def col: Config => Mongo => BSONCollection

  def findOne(query: BSONDocument)(mongo: Mongo)(implicit ev: BSONDocumentReader[A]) =
    liftM((c: Config) => col(c)(mongo).find(query).one[A].right[String])

  def insert(a: A)(mongo: Mongo)(implicit ev: BSONDocumentWriter[A]) =
    liftM(col(_)(mongo).insert(a).right[String])

  def insert2(a: A) = liftM(col(_).right[String]).
    flatMap(f =>
    Kleisli[ErrorM, Mongo, Future[LastError]]((mongo: Mongo) => f(mongo).insert(a).right[String]))

  def insert3(a: A) = liftM(c =>
    Kleisli[ErrorM, Mongo, Future[LastError]](m =>
      col(c)(m).insert(a).right[String]).right[String])

  def insert4 = ((c: Config) => 1.right[String]).pure[Kleisli[ErrorM, Config, Int]]
}

sealed trait Mongo {
  val logger = LoggerFactory.getLogger(classOf[Mongo])

  val config: Config
  lazy val driver = MongoDriver()
  lazy val conn = driver.connection(config.opt[List[String]]("mongo.connection").getOrElse(List("127.0.0.1")))
}


object Mongo  {
  val logger = LoggerFactory.getLogger(classOf[Mongo])
  type UnfilteredMongoM = UnfilteredM[Mongo]
  def unfilteredMongoM = {
    liftM[Mongo]((c:Config) => {
      (new Mongo {
        val config = c
      }).right[String]
    })
  }
}
