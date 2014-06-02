package com.logikujo

//import com.logikujo.www.plans.{NotFoundPlan, RootPlan}
import unfiltered.jetty.{ContextBuilder, Server, Http}
import com.github.kxbmap.configs._
import unfiltered.filter._
import unfiltered.directives.{Result, Directive}
import unfiltered.response.ResponseFunction
import unfiltered.request.HttpRequest
import com.typesafe.config.{ConfigFactory, Config}
import scalaz._
import Scalaz._

/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www 10/05/14 :: 17:38 :: eof
 *
 */
package object www {
  object Implicits {
    implicit def test[A,B](i:Plan.Intent):UnfilteredPlan = new UnfilteredPlan {
      def intent = i
    }
    implicit def toUnfilteredPlan(intentM:UnfilteredIntentM):UnfilteredPlanM =
      intentM map (i => new UnfilteredPlan {def intent = i})
    implicit def toUnfilteredIntentM(intent: Plan.Intent): UnfilteredIntentM =
      unfilteredIntentM(_ => intent.right)
    //implicit def toUnfilteredConfigM(c: Config): UnfilteredConfigM =
    //  unfilteredConfigM(_ => c.right)
  }
  type ErrorM[+A] = String \/ A
  type UnfilteredM[+A] = ReaderT[ErrorM, Config, A]
  type UnfilteredPlanM = UnfilteredM[UnfilteredPlan]
  type UnfilteredIntentM = UnfilteredM[Plan.Intent]
  type UnfilteredApp = UnfilteredM[Server]
  type UnfilteredConfigM = UnfilteredM[Config]
  type DirectiveAny[A,B] = Directive[Any, ResponseFunction[A], B]
  type DirectiveM[+A] = Directive[Any, ResponseFunction[Any], A]
  type UnfilteredDirectiveM[+A] = ReaderT[DirectiveM, Config, A]
  def liftM[A](f: Config => ErrorM[A]) = Kleisli[ErrorM, Config, A](f)
  def unfilteredM = liftM[Server]_
  def unfilteredPlanM = liftM[UnfilteredPlan]_
  def unfilteredIntentM = liftM[Plan.Intent]_
  def unfilteredConfigM = Kleisli.ask[ErrorM, Config]

  // ReaderT overResultM, ResponseFunction[Any]
  type ResultM[+A] = Result[ResponseFunction[Any],A]
  def directiveM[A](directive: Directive[Any, ResponseFunction[Any], A]) =
    Kleisli[ResultM, HttpRequest[Any], A](r => directive(r))
  //def directive[A](directiveM: Kleisli[ResultM, HttpRequest[Any],A]) =
  //  Directive[Any, Any, A](directiveM.run(_))


  trait UnfilteredPlan extends Plan

  object UnfilteredApp {
    def apply(): UnfilteredApp = unfilteredM(
      (c: Config) => (for {
        serverPort <- c.get[Option[Int]]("serverPort").orElse(8080.some)
        assetsMap <- c.get[Option[String]]("assetsMap").orElse("/assets".some)
        assetsDir <- c.get[Option[String]]("assetsDir").orElse("/www/css".some)
      } yield Http(serverPort).context(assetsMap) { ctx =>
        ctx.current.setInitParameter("org.eclipse.jetty.servlet.Default.dirAllowed", "false")
        ctx.resources(new java.net.URL(getClass().getResource(assetsDir), "."))
      }).\/>("Unable to create server"))
  }

  object UnfilteredPlan {
    def apply(f: Config => ErrorM[Plan.Intent]): UnfilteredPlanM = unfilteredPlanM(
      (c: Config) => f(c).map(i => new UnfilteredPlan {
        def intent = i
      })
    )
    def apply(i:Plan.Intent): UnfilteredPlanM = apply((c:Config) => i.right[String])
    //def apply(f: Config => Plan.Intent): UnfilteredPlanM = apply((c:Config) => f(c).right[String])
  }

  trait DirectiveMOps[A] {
    val value: UnfilteredDirectiveM[A]
  }

  // Ops over UnfilteredApp
  trait UnfilteredMOps {
    val value: UnfilteredApp
    def ~>(mPlans: (String, List[UnfilteredPlanM])): UnfilteredApp = {
      val (ctx, plans) = mPlans
      for {
        server <- value
        planList <- plans.sequence
      } yield server.context(ctx)((planList.foldLeft(_: ContextBuilder)(_ filter _)).andThen(_ => Unit))
    }
    def run(path:Option[String] = None) = value map (_.run()) run ConfigFactory.load.getConfig(path.getOrElse(""))
  }

  implicit def unfilteredApp2Ops(v:UnfilteredApp): UnfilteredMOps =
    new UnfilteredMOps {
      val value = v
    }
}
