package com.logikujo.www

import scala.util.Try
import com.logikujo.www.data.TriedDirective
import java.io._
import java.io.{Writer => JWriter}
import scala.util.Failure
import scala.util.Success
import org.fusesource.scalate._
import org.fusesource.scalate.util.Logging
import com.typesafe.config.Config
import com.github.kxbmap.configs._
import unfiltered.response._
import unfiltered.request._
import org.fusesource.scalate.layout.DefaultLayoutStrategy
import unfiltered.directives._
import Directives._
import scalaz._
import Scalaz._

/*
 *  TODO: Support for several engines, now is hardcoded
 */

package object scalate {
  type UnfilteredScalateM = UnfilteredM[Scalate]

  def scalateM[App]: App #> Scalate = #> { c => new Scalate {
    val config = c
  }.right[String]
  }

  abstract class ScalateRender {
    val config: Configuration
    val success: String => ResponseFunction[Any]
    val failure: Throwable => ResponseFunction[Any]
    type ToRenderContext = (String, JWriter, TemplateEngine) => Try[DefaultRenderContext]
    lazy val defaultDir: File = config.config.get[Option[String]]("scalate.prefix").fold(new File(""))(new File(_))
    lazy val defaultTemplatesDir = config.opt[List[String]]("scalate.templatesDir").fold(Seq(defaultDir))(_.map(new File(_)))
    lazy val defaultLayoutsDir = config.opt[String]("scalate.layoutsDir").fold(new File(defaultDir, "layouts/"))(new File(_))
    lazy val defaultLayout = config.opt[String]("scalate.defaultLayout").getOrElse(defaultLayoutsDir.toString) + "/default.scaml"

    // Needs to be lazy!!! Otherwise option is null!
    lazy val runMode = config.get[Option[String]]("scalate.runMode").getOrElse("production")
    lazy val developmentMode = runMode == "development"

    //val runMode: String = (new SystemProperties).getOrElseUpdate("runMode","production")
    val withLayoutSupport: Boolean = true
    private def contextBuilder(path: String, engine: TemplateEngine) = (writer: JWriter) =>
      new DefaultRenderContext(path, engine, new PrintWriter(writer))
    private def renderPage(engine: TemplateEngine,
                           contextForWriter: JWriter => DefaultRenderContext,
                           template: String,
                           attributes: Seq[(String, Any)],
                           out: StringWriter = new StringWriter)
                          (implicit additionalAttributes: Seq[(String, Any)]) = Try {
      val context = contextForWriter(out)
      (additionalAttributes ++ attributes) foreach { case (k, v) => context.attributes(k) = v}
      engine.layout(template, context)
      out
    }.transform(_ => {
      out.close()
      Success(out)
    }, t => {
      out.close()
      Failure(t)
    }).map(_.toString)

    protected def renderString[A, B](request: HttpRequest[A],
                           template: String,
                           attributes: Seq[(String, Any)])
                          (implicit
                           bindings: List[Binding] = Nil,
                           additionalAttributes: Seq[(String, Any)] = Nil
                            ) = {
      val renderedString = for {
        context <- Try {
          contextBuilder(Path(request), defaultEngine)
        }
        page <- renderPage(defaultEngine, context, template, attributes)
      } yield page
      renderedString match {
        case Success(page) => success(page)
        case Failure(t) if defaultEngine.isDevelopmentMode =>
          val sw = new StringWriter()
          t.printStackTrace(new PrintWriter(sw))
          InternalServerError ~> ResponseString(sw.toString)
        case Failure(t) => failure(t)
      }
    }

    lazy private val defaultEngine = {
      val engine = new TemplateEngine(defaultTemplatesDir, runMode)
      if (withLayoutSupport) engine.layoutStrategy =
        new DefaultLayoutStrategy(engine, defaultLayout)
      if (!developmentMode) {
        engine.allowReload = false
        engine.allowCaching = true
        engine.packagePrefix = "webTmpl"
      }
      engine
    }
  }
  sealed trait Scalate extends ScalateRender {
    val success: String => ResponseFunction[Any] =
      Ok ~> ResponseString(_)
    val failure: Throwable => ResponseFunction[Any] =
      _ => InternalServerError ~> ResponseString("E500: InternalServerError")
    def withSuccess(f: String => ResponseFunction[Any]) = {
      val _failure = this.failure
      val _config = this.config
      new Scalate {
        val config: Configuration = _config
        override val success = f
        override val failure = _failure
      }
    }
    def withFailure(f: Throwable => ResponseFunction[Any]) = {
      val _config = this.config
      val _success = this.success
      new Scalate {
        val config: Configuration = _config
        override val success = _success
        override val failure = f
      }
    }
    def apply[A](request: HttpRequest[A], template: String, attributes: (String, Any)*) =
      renderString(request, template, attributes)
    def render[A](request: HttpRequest[A], template: String, attributes: (String,Any)*) =
      apply(request, template, attributes:_*)
  }
}