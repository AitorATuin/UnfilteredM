package com.logikujo.www

import reactivemongo.bson._
import argonaut._
import Argonaut._
import org.joda.time.DateTime
import scala.concurrent.Future
import reactivemongo.api.collections.default.BSONCollection
import scala.concurrent.ExecutionContext.Implicits.global

/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www 29/01/14 :: 21:08 :: eof
 *
 */
package object model {
  /*
   * DateTime Reader and Writer for reactivemongo
   * DateTime EncodeJson and DecodeJson for argonaut
   */
  object Implicits {
    implicit object DatetimeReader extends BSONReader[BSONDateTime, DateTime] {
      def read(bson: BSONDateTime): DateTime = new DateTime(bson.value)
    }
    implicit object DatetimeWriter extends BSONWriter[DateTime, BSONDateTime] {
      def write(t: DateTime): BSONDateTime = BSONDateTime(t.getMillis)
    }
    implicit def DateTimeEncodeJson: EncodeJson[DateTime] =
      EncodeJson((d:DateTime) => jNumber(d.getMillis))
    implicit def DateTimeDecodeJson: DecodeJson[DateTime] =
      DecodeJson(c => c.as[Long].map(new DateTime(_)))
  }
  trait RestRecord[A] {
    def id(a:A): String
    def onCreate(a:A): Json
  }
  trait DAOQuery[A] {
    def query(a:A)
  }
  trait InsertDAO[A] {
    def insert[A](a:A): ErrorM[A]
  }

  trait DBDAO[DB] {
    def insert[A](a:A) : A
  }

  // TODO: Allow several types on the query tuple, now only Strings are allowed
  trait DAO {
    //def insert[A](a:A): ErrorM[A]
    //def update[A](a:A): ErrorM[A]
    //def delete[A](a:A): ErrorM[A]
    //def read[A](query:(String, Any)*): ErrorM[A]
    //def findOne[DB](query:(String, String)*): Future[Option[A]]
    //def insert[DB](implicit ev: DBDAO[DB])// = ev.insert _ //(a:A): Future[ErrorM[A]]
  }
}
