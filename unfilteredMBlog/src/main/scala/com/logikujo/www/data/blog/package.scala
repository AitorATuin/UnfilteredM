package com.logikujo.www
package data

import model.blog._
import error.blog._

import scalaz._
import Scalaz._
import unfiltered.directives._
import unfiltered.request._
import unfiltered.response._
import unfiltered.filter._
import Directives._
import dispatch._
import Defaults._
import scala.util.Try
import argonaut._
import Argonaut._

/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.data 8/02/14 :: 15:51 :: eof
 *
 */
package object blog {
  import unfiltered.directives.{data => unData}
  import unfiltered.directives._
  import Directives._
  private type VPostEntry = \/[NonEmptyList[String], PostEntry]
  private val getUrlF = Try {(s:String) => Http.configure(_ setFollowRedirects true)(url(s) OK as.String)}
  private val asVPostEntry = unData.Fallible[String, VPostEntry](s => {
    val k = getUrlF.map(_(s)).map(_ map (PostEntry.fromString(_))) map(_())
    k.toOption
  })

  private val vPostEntryValue =
    unData.as.String ~> asVPostEntry.fail {
      (k, v) => BlogBadRequest(("url" := "Bad url request.") ->: jEmptyObject)
    }
  private val asPostEntry = unData.Fallible[VPostEntry, PostEntry](_.toOption)
  implicit def required[PostEntry] = data.Requiring[PostEntry].fail {
    f => BlogBadRequest(("url" := "'url' field is required.") ->: jEmptyObject)
  }
  implicit def implyPostEntry =
    vPostEntryValue ~> asPostEntry.fail((k,v) => {
      // TODO: Format list into json sorted by field
      BlogBadRequest(jString(v.swap.getOrElse("Unknow Error".wrapNel).toList.toString))
    })
}
