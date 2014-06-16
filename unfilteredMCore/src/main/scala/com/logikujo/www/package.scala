package com.logikujo

//import com.logikujo.www.plans.{NotFoundPlan, RootPlan}
import unfiltered.jetty.{ContextBuilder, Server, Http}
import com.github.kxbmap.configs._
import unfiltered.Cycle._
import unfiltered.Async
import unfiltered.filter._
import unfiltered.directives.{Result, Directive}
import unfiltered.response.ResponseFunction
import unfiltered.request.HttpRequest
import com.typesafe.config.{ConfigException, ConfigFactory, Config => CConfig}
import scalaz._
import Scalaz._
import javax.servlet.{ServletResponse, ServletRequest}
import javax.servlet.http.{HttpServletRequest, HttpServletResponse}
import scala.util.{Try, Success => TSuccess, Failure => TFailure}

/**
 *
 * aitoriturri / LogiDev - [Fun Functional] / Logikujo.com
 *
 * com.logikujo.www 10/05/14 :: 17:38 :: eof
 *
 */
package object www {
  object Implicits {
    implicit def toUnfilteredPlan(intentM:UnfilteredIntentM):UnfilteredPlanM =
      intentM map (i => new UnfilteredPlan {def intent = i})
    implicit def toUnfilteredIntentM(intent: Plan.Intent): UnfilteredIntentM =
      unfilteredIntentM(_ => intent.right)
  }

  // Confuration trait
  trait Configuration {
    val config: CConfig
    def get[T: AtPath](path: String): T = config.get[T](path)
    def opt[T: AtPath](path: String): Option[T] = config.opt[T](path)
    def disjunction[T: AtPath](path: String): \/[String, T] =
      config.opt[T](path).\/>(s"Not found config value: ${path}")
    def ∨[T: AtPath](path: String) = disjunction[T](path)
    def atPath[Tag](path: String) = Try(config.atPath(path)).
      toOption.\/>(s"Not found config path: ${path}").
      map(Configuration[Tag](_))
    def resolv[A,T](implicit ev: A @@ T) = ev
    def resolvFM[Tag, A, B, T](implicit ev:Config[Tag] => ErrorM[A => (B @@ T)]) = #>(ev)
    def resolvM[Tag, A, T](implicit ev:Config[Tag] => ErrorM[A @@ T]) = #>(ev)
  }

  trait TaggedOps[A] {
    val v:A
    def withTag[Tag]: A @@ Tag = v.asInstanceOf[A @@ Tag]
  }

  implicit def asTaggedOps[A](a: A) = new TaggedOps[A] {
    val v:A = a
  }

  // TODO: apply can throw exception if path is not found.
  object Configuration {
    //def withTag[Tag](c:Configuration): Configuration @@ Tag = c.asInstanceOf[Configuration @@ Tag]
    /*def apply[Tag](path: String) = new Configuration {
      val config = ConfigFactory.load.getConfig(path)
    }.asInstanceOf[Configuration @@ Tag]*/
    def apply[Tag](path: String) = new Configuration {
      val config = ConfigFactory.load.getConfig(path)
    }.withTag[Tag]
    def apply[Tag](c: CConfig) = new Configuration {
      val config = c
    }.withTag[Tag]
    def apply[Tag]() = new Configuration {
      val config = ConfigFactory.empty()
    }.withTag[Tag]
  }
  type Config[Tag] = Configuration @@ Tag

  // Reader Monad with Tagged Configuration
  type #>[Tag, R] = Kleisli[ErrorM, Config[Tag], R]
  type ##>[C, Tag, R] = Kleisli[ErrorM, C @@ Tag, R]
  type ?>[Tag, R] = (Configuration @@ Tag) => ErrorM[R]
  type ??>[C, Tag, R] = (C @@ Tag) => ErrorM[R]
  //type ??>[Tag, A, B, R] = (Configuration @@ Tag) => (A => (B @@ R))

  // alias for \/ monad with String in the left.
  type ErrorM[+A] = String \/ A
  type UnfilteredM[+A] = ReaderT[ErrorM, Configuration, A]
  type UnfilteredPlanM = UnfilteredM[UnfilteredPlan]
  type UnfilteredIntentM = UnfilteredM[Plan.Intent]
  type UnfilteredApp = UnfilteredM[Server]
  type UnfilteredConfigM = UnfilteredM[Configuration]
  type DirectiveAny[A,B] = Directive[Any, ResponseFunction[A], B]
  type DirectiveM[+A] = Directive[Any, ResponseFunction[Any], A]
  type UnfilteredDirectiveM[+A] = ReaderT[DirectiveM, Configuration, A]

  // Lift a function from Tagged Configuration to Kleisli
  def #>[Tag, R](f: Config[Tag] => ErrorM[R]): Tag #> R = Kleisli[ErrorM, Config[Tag], R](f)
  def #>[Tag, R](v: R): Tag #> R = #>(_ => v.right[String])

  // Lift a function from Tagger C to Kleisli
  def ##>[C, Tag, R](f: C @@ Tag => ErrorM[R]) = Kleisli[ErrorM, C @@ Tag, R](f)
  def ##>[C, Tag, R](v: R): ##>[C, Tag, R] = ##>(_ => v.right[String])

  def liftM[A](f: Configuration => ErrorM[A]) = Kleisli[ErrorM, Configuration, A](f)
  // Just returns the configuration, ask function inside Reader Monad
  def _configM = Kleisli.ask[ErrorM, Configuration]
  def configM[Tag] = Kleisli.ask[ErrorM, Config[Tag]]

  def unfilteredM = liftM[Server]_
  def unfilteredPlanM = liftM[UnfilteredPlan]_
  def unfilteredIntentM = liftM[Plan.Intent]_

  // ReaderT overResultM, ResponseFunction[Any]
  type ResultM[+A] = Result[ResponseFunction[Any],A]
  def directiveM[A](directive: Directive[Any, ResponseFunction[Any], A]) =
    Kleisli[ResultM, HttpRequest[Any], A](r => directive(r))
  //def directive[A](directiveM: Kleisli[ResultM, HttpRequest[Any],A]) =
  //  Directive[Any, Any, A](directiveM.run(_))


  trait UnfilteredPlan extends Plan

  object UnfilteredApp {
    def apply[App](): App #> Server = #> { (c: Config[App]) =>
      (for {
        serverPort <- c.get[Option[Int]]("serverPort").orElse(8080.some)
        assetsMap <- c.get[Option[String]]("assetsMap").orElse("/assets".some)
        assetsDir <- c.get[Option[String]]("assetsDir").orElse("/www/css".some)
      } yield Http(serverPort).context(assetsMap) { ctx =>
        ctx.current.setInitParameter("org.eclipse.jetty.servlet.Default.dirAllowed", "false")
        ctx.resources(new java.net.URL(getClass().getResource(assetsDir), "."))
      }).\/>("Unable to create server")}
  }

  object UnfilteredPlan {
    def apply(f: Configuration => ErrorM[Plan.Intent]): UnfilteredPlanM = unfilteredPlanM(
      (c: Configuration) => f(c).map(i => new UnfilteredPlan {
        def intent = i
      })
    )
    def apply(i:Plan.Intent): UnfilteredPlanM = apply((c:Configuration) => i.right[String])
    //def apply(f: Config => Plan.Intent): UnfilteredPlanM = apply((c:Config) => f(c).right[String])
  }

  trait UnfilteredMAs[A,B] {
    def as(a:A):B
  }

  trait UnfilteredMAsOps[Tag, A] {
    def value: Tag #> A
    def as[B](implicit ev: UnfilteredMAs[A,B]): Tag #> B = value map ev.as _
  }

  implicit def asUnfilteredMAsOps[Tag, A](v: Tag #> A) = new UnfilteredMAsOps[Tag, A] {
    def value = v
  }

  implicit object intentMAsPlanM1 extends UnfilteredMAs[unfiltered.Cycle.Intent[Any,Any], Plan] {
    def as(a: unfiltered.Cycle.Intent[Any,Any]): Plan = new Plan { def intent = a }
  }
  implicit object intentMAsPlanM2 extends UnfilteredMAs[Plan.Intent, Plan] {
    def as(a: Plan.Intent): Plan = new Plan { def intent = a }
  }
  implicit object intentMAsPlanM3 extends UnfilteredMAs[Async.Intent[HttpServletRequest, HttpServletResponse], async.Plan] {
    def as(a:Async.Intent[HttpServletRequest, HttpServletResponse]): async.Plan = new async.Plan {def intent = a}
  }

  implicit object planAsInittedFilter extends UnfilteredMAs[Async.Intent[HttpServletRequest, HttpServletResponse], InittedFilter] {
    def as(a:Async.Intent[HttpServletRequest, HttpServletResponse]): InittedFilter = a.asInstanceOf[InittedFilter]
  }
  /*implicit object intentMAsPlanM2 extends UnfilteredMAs[unfiltered.filter.Plan.Intent[Any,Any], Plan] {
    def as(a: unfiltered.filter.Plan.Intent[Any,Any]): Plan = new Plan { def intent = a }
  }*/

  trait ServerMOps[App] {
    val value: App #> Server
    def ~>(mPlans: (String, List[App #> async.Plan.Intent])): App #> Server = {
      val (ctx, intents) = mPlans
      for {
        server <- value
        intentsList <- intents.sequenceU
      } yield server.context(ctx){_.filter(async.Planify(intentsList.reduce(_ orElse _))) } // TODO: Try to use as[sync.Plan]
    }
    def run()(implicit c: Config[App]) = value map (_.run()) run c
  }

  implicit def asServerMOps[App](v:App #> Server): ServerMOps[App] =
    new ServerMOps[App] {
      val value = v
    }
}
