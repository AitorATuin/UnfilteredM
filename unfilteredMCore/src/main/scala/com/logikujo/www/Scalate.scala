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

package object scalate {
  type UnfilteredScalateM = UnfilteredM[Scalate]
  def unfilteredScalateM = liftM[Scalate]((c:Configuration) => (new Scalate {
    val config = c
  }).right[String])

  sealed trait Scalate extends Logging {
    val config: Configuration
    type ToRenderContext =
    (String, JWriter, TemplateEngine) => Try[DefaultRenderContext]
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

    //private def renderPage(template: String) = ()
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
      out.close();
      Success(out)
    }, t => {
      out.close();
      Failure(t)
    }).map(_.toString)

    def renderScalate[A, B](request: HttpRequest[A],
                            template: String,
                            attributes: (String, Any)*)
                           (implicit
                            engine: TemplateEngine = defaultEngine,
                            bindings: List[Binding] = Nil,
                            additionalAttributes: Seq[(String, Any)] = Nil
                             ) = {
      val renderedString = for {
      //te <- Try { engine.load(template, bindings)}
        context <- Try {
          contextBuilder(Path(request), engine)
        }
        page <- renderPage(engine, context, template, attributes)
      } yield page
      TriedDirective.successOrElse(renderedString, {
        case e: Throwable if engine.isDevelopmentMode =>
          val str = new StringWriter
          e.printStackTrace(new PrintWriter(str))
          str.close()
          InternalServerError ~> ResponseString(str.toString)
        case e: Throwable => InternalServerError ~> ResponseString("No implementado!")
      })
    }

    def render(path: String, attributes: (String, Any)*) = for {
      _ <- GET
      r <- Directives.request[Any]
      page <- renderScalate(r, path, attributes: _*)
    }  yield page

    def apply(path: String, attributes: (String, Any)*): Directive[Any, ResponseFunction[Any], ResponseFunction[Any]] =
      render(path, attributes:_*).map(Ok ~> ResponseString(_))
       /* _ <- GET
        r <- Directives.request[Any]
        page <- renderScalate(r, path, attributes: _*)
      } yield Ok ~> ResponseString(page)*/


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
}

/*object Scalate {
  def apply():UnfilteredScalateM = unfilteredScalateM((c:Config) => (new Scalate {
    val config = c
  }).right[String])
}*/