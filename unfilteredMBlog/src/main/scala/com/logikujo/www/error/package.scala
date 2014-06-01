package com.logikujo.www
package error

import model._

import unfiltered.response._
import unfiltered.request._
import org.joda.time.DateTime
import scalaz._
import argonaut._
import Argonaut._

/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.data.blog 29/01/14 :: 20:27 :: eof
 *
 */

package object blog {
  val BlogSuccess = (responseStr: Json) => JsonResponseSuccess(responseStr, Ok)
  def BlogPostCreated[A](a: A)(implicit ev: RestRecord[A]) =
    Location("/api/v1/posts" + ev.id(a)) ~> JsonResponseSuccess(ev.onCreate(a), Created)
    /*Location("/api/v1/posts/" + post.id) ~>
    RestResponseSuccess(
      ("title" := post.title) ->: ("id" := post.id) ->: jEmptyObject,
      Created
    )*/
  //val BlogPostCreated = ServerError
  def BlogPostNotFound = JsonResponseFail(
    ("clean_title" := "post not found") ->: jEmptyObject,
    NotFound)
  val BlogBadRequest = (err: Json) => JsonResponseFail(err, BadRequest)
  val BlogPostConflict =
    JsonResponseFail(
      (("title" := "Title exists in the data base") ->: jEmptyObject),
      Conflict)
}

