package com.logikujo.www

import scala.xml.NodeSeq
import java.util.UUID._

/**
 *
 * my-web-project / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www.captcha 22/04/14 :: 22:04 :: eof
 *
 */
package object captcha {
  trait Captcha[A] {
    def question: NodeSeq
    def solve(answer:A): Boolean
  }
  trait CaptchaFactory[C, A] {
    var captchas: Map[String, A => Boolean] = Map.empty
    def creator: Captcha[A]
    lazy val captcha: Option[Captcha[A]] = Some(create)
    def create: Captcha[A] = if (captcha.isDefined) captcha.get else {
      captchas + (java.util.UUID.randomUUID.toString -> captcha.get.solve _)
      captcha.get
    }
  }
}
