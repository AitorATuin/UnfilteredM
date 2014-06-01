package dispatch.as.argonaut.stream

/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * argonaut.stream 16/02/14 :: 23:16 :: eof
 *
 */

import dispatch.stream.StringsByLine

import scalaz._, Scalaz._
import argonaut._ , Argonaut._
import argonaut.Json

object Json {
  def apply[T](f: Json => T) =
    new StringsByLine[Unit] {
      def onStringBy(string: String) {
        Parse.parse(string) map f
      }
      def onCompleted() = ()
    }
}