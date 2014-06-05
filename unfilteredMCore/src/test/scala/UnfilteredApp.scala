package com.logikujo.www
/**
 *
 * UnfilteredM / LogiDev - [Fun Functional] / Logikujo.com
 *
 * 5/06/14 :: 20:15 :: eof
 *
 */

import scalate._
import org.scalatest._
import org.scalatest.matchers._
import unfiltered.jetty.Server
import unfiltered.response._
import unfiltered.filter._

abstract class UnitSpec
  extends FlatSpec
  with ShouldMatchers
  with OptionValues
{
  trait TestApp

  def anInstanceOf[T](implicit tag: reflect.ClassTag[T]) = {
    val clazz = tag.runtimeClass
    new BePropertyMatcher[AnyRef] {
      def apply(left: AnyRef) =
        BePropertyMatchResult(clazz.isAssignableFrom(left.getClass),
          "an instance of " + clazz.getName)
    }
  }
}

class UnfilteredTest extends UnitSpec {
  behavior of "a UnfilteredApp"

  it should "be able to be constructed, giving a TestApp #> Server" in {
    val server = UnfilteredApp[TestApp]()
    server should be (anInstanceOf[TestApp #> Server])
    server should not be (anInstanceOf[List[_]])
  }

  behavior of "a intentM"

  it should "be able to be created, giving a TestApp #> Intent" in {
    val intentM = for {
      config <- configM
    } yield unfiltered.filter.Intent {
        case _ => ResponseString("test")
      }
    intentM should be (anInstanceOf[TestApp #> Plan.Intent])
  }

  it should "be able to be used as a TestApp #> Plan" in {
    intentM.as[Plan] should be (anInstanceOf[TestApp #> Plan])
  }
}
