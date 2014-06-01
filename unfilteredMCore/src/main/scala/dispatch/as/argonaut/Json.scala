package dispatch.as.argonaut

/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * argonaut 16/02/14 :: 23:16 :: eof
 *
 */
import com.ning.http.client.Response

import scalaz._, Scalaz._
import argonaut._, Argonaut._

object Json extends (Response => String \/ Json) {
  def apply(r: Response) = (dispatch.as.String andThen (Parse.parse(_)))(r)
}