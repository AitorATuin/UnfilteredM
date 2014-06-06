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
import unfiltered.Cycle
import unfiltered.jetty.Server
import unfiltered.response._
import unfiltered.filter._
import unfiltered.directives._
import Directives._

import scalaz._
import Scalaz._

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

  trait UnfilteredAppTest {
    val serverM = UnfilteredApp[TestApp]()
  }

  // TODO: Allow #>(C => R) with R != \/
  trait UnfilteredIntentTest {
    // Cycle Intents
    val intentCycleM1 = for {
      config <- configM[TestApp]
    } yield Directive.Intent[Any,Any] {
        case _ => success(ResponseString("intentCycleM1 Test"))
    }
    val intentCycleM2 = #>[TestApp, Cycle.Intent[Any,Any]]((_:Config[TestApp]) =>
      Directive.Intent[Any,Any] {
        case _ => success(ResponseString("intentCycleM2 Test"))
      }.right[String])

    val intentCycleM3 = #>[TestApp, Cycle.Intent[Any, Any]](Directive.Intent[Any,Any] {
      case _ => success(ResponseString("intentCycleM3 Test"))
    })

    // Plain Intents
    val intentPlanM1 = for {
      config <- configM[TestApp]
    } yield unfiltered.filter.Intent {
      case _ => ResponseString("intentPlanM1 Test")
    }
    val intentPlanM2 = #>[TestApp, Plan.Intent]((_:Config[TestApp]) =>
      Intent {
        case _ => ResponseString("intentPlanM2 Test")
      }.right[String])

    val intentPlanM3 = #>[TestApp, Plan.Intent](Intent {
      case _ => ResponseString("intentPlanM3 Test")
    })
  }

  trait TaggedKleisli {
    trait Trait1
    trait Trait2
    trait Tag1
    trait Tag2

    val tagged1 = ##>[Trait1, Tag1, Int]{(t: Trait1 @@ Tag1) => 2.right[String]}
    val tagged2 = ##>[Trait1, Tag2, Plan.Intent](Intent {
      case _ => ResponseString("taggedIntent2 Test")
    })
    val tagged3 = ##>[Trait2, Tag1, Cycle.Intent[Any, Any]](Directive.Intent[Any, Any] {
      case _ => success(ResponseString("taggedIntent3 Test"))
    })
    val tagged4 = ##>[Trait2, Tag2, Cycle.Intent[Any, Any]](
      (c: Trait2 @@ Tag2) => Directive.Intent[Any, Any] {
        case _ => success(ResponseString("taggedIntent4 Test"))
      }.right[String]
    )
  }

  "UnfilteredApp" should
    "be able to be constructed, giving a TestApp #> Server" in new UnfilteredAppTest {
      serverM should be (anInstanceOf[TestApp #> Server])
      serverM should not be (anInstanceOf[List[_]])
  }

  "IntentM - Cycle Intent" should
    "be able to be created in several ways, resulting in a TestApp #> Intent" in new UnfilteredIntentTest {
      intentCycleM1 should be (anInstanceOf[TestApp #> Cycle.Intent[Any,Any]])
      info("for syntax")
      intentCycleM1 should be (anInstanceOf[TestApp #> Cycle.Intent[Any,Any]])
      info("#>(function) syntax")
  }

  it should "be able to be used as a TestApp #> Plan" in new UnfilteredIntentTest {
    intentCycleM1.as[Plan] should be(anInstanceOf[TestApp #> Plan])
    intentCycleM2.as[Plan] should be(anInstanceOf[TestApp #> Plan])
  }

  "IntentM - Plan Intent" should
    "be able to be created in several ways, resulting in a TestApp #> Intent" in new UnfilteredIntentTest {
      intentPlanM1 should be (anInstanceOf[TestApp #> Plan.Intent])
      info("for syntax")
      intentPlanM2 should be (anInstanceOf[TestApp #> Plan.Intent])
      info("#>(function) syntax")
  }

  it should "be able to be used as a TestApp #> Plan" in new UnfilteredIntentTest{
    intentPlanM1.as[Plan] should be (anInstanceOf[TestApp #> Plan])
    intentPlanM2.as[Plan] should be (anInstanceOf[TestApp #> Plan])
  }

  "Kleisli with tagged values" should
    "be able to be created, resulting in ##>[Type, Tag, Result]" in new TaggedKleisli {
      tagged1 should be (anInstanceOf[##>[Trait1, Tag1, Int]])
      tagged2 should be (anInstanceOf[##>[Trait1, Tag2, Plan.Intent]])
      tagged3 should be (anInstanceOf[##>[Trait2, Tag1, Cycle.Intent[Any, Any]]])
      tagged3 should be (anInstanceOf[##>[Trait2, Tag2, Cycle.Intent[Any, Any]]])
  }
}
