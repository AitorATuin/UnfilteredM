import sbt._
import sbt.Keys._

import less.Plugin._
import sbtlivescript.SbtLiveScriptPlugin._
import LiveScriptKeys._
import sbtclosure.SbtClosurePlugin._
import spray.revolver.RevolverPlugin._
import sbtassembly.Plugin.AssemblyKeys._
import sbtassembly.Plugin._
import com.mojolly.scalate.ScalatePlugin._
import ScalateKeys._

trait WithDependencies {
  val dependenciesBuild = Seq(
//    "com.logikujo" %% "unfilteredm-core" % "0.2-SNAPSHOT",
//    "com.logikujo" %% "unfilteredm-mail" % "0.2-SNAPSHOT",
//    "com.logikujo" %% "unfilteredm-captcha" % "0.2-SNAPSHOT",
//    "com.logikujo" %% "unfilteredm-blog" % "0.2-SNAPSHOT"
//    "org.reactivemongo" %% "reactivemongo" % "0.11.0-SNAPSHOT"
  )
}

trait WithResolvers {
  val resolversBuild = Seq(
  "java m2" at "http://download.java.net/maven/2",
  "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
  "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"
)
}

object BuildSettings {

  private def mergeFunc(old: String => MergeStrategy): (String) => MergeStrategy = {
    case PathList("META-INF", xs@_*) =>
      (xs map {
        _.toLowerCase
      }) match {
        case ("eclipsef.sf" :: Nil) => MergeStrategy.rename
        case ("mime.types" :: Nil) => MergeStrategy.discard
        case ("manifest.mf" :: Nil) => MergeStrategy.discard
        case ("dependencies" :: Nil) => MergeStrategy.discard
        case ("license" :: Nil) | ("license.txt" :: Nil) => MergeStrategy.discard
        case ("notice" :: Nil) | ("notice.txt" :: Nil) => MergeStrategy.discard
        case _ => MergeStrategy.deduplicate
      }
    case x => old(x)
  }

  val buildTime = SettingKey[String]("build-time")

  val basicSettings = Defaults.defaultSettings ++ Seq(
    name := "AppTest",
    version := "0.1-SNAPSHOT",
    organization := "com.logikujo",
    scalaVersion := "2.10.4",
    scalacOptions <<= scalaVersion map { sv: String =>
      if (sv.startsWith("2.10."))
        Seq("-deprecation", "-unchecked", "-feature", "-language:postfixOps", "-language:implicitConversions")
      else
        Seq("-deprecation", "-unchecked")
    },
    javaOptions ++= List("-Dcom.logikujo.atuinsapp.scalate.runMode=production"),
    fork in run := true
  )

  val lessSettingsBuild = lessSettings ++ Seq(
    (LessKeys.filter in (Compile, LessKeys.less)) := "*styles.less",
    (LessKeys.mini in (Compile, LessKeys.less)) := true,
    (resourceManaged in (Compile, LessKeys.less)) <<= (resourceDirectory in Compile)(_ / "www" / "css")
  )

  val liveScriptSettingsBuild = liveScriptSettings ++ Seq(
    (outputDirectory in (Compile, livescript)) := (sourceDirectory in (Compile, ClosureKeys.closure)).value / "ls"
  )

  val closureSettingsBuild = closureSettings ++ Seq(
    (ClosureKeys.closure in Compile) := ((ClosureKeys.closure in Compile) dependsOn (livescript in (Compile, livescript))).value,
    (resourceManaged in (Compile, ClosureKeys.closure)) <<= (resourceDirectory in Compile)(_ / "www" / "js"),
    (ClosureKeys.prettyPrint in (Compile, ClosureKeys.closure)) := true
  )

  val revolverSettingsBuild = Revolver.settings ++ Seq(
    javaOptions in Revolver.reStart += "-Djavax.net.ssl.trustStore=src/main/resources/cacerts.jks",
    javaOptions in Revolver.reStart += "-Djavax.net.ssl.trustStorePassword=changeit",
    javaOptions in Revolver.reStart += "-Dcom.logikujo.apptest.scalate.runMode=development",
    javaOptions in Revolver.reStart += "-Dcom.logikujo.apptest.scalate.prefix=src/main/resources/scalate"
  )

  val assemblySettingsBuild = assemblySettings ++ Seq(
    mainClass in assembly := Some("com.logikujo.www.AppTest"),
    mergeStrategy in assembly <<= (mergeStrategy in assembly)(mergeFunc(_))
  )

  val scalateSettingsBuild = scalateSettings ++ Seq(
    scalateTemplateConfig in Compile := {
      val base = sourceDirectory.in(Compile).value
      Seq(
        TemplateConfig(
          base / "resources" / "scalate",
          Seq(
          ),
          Seq(
          ),
          Some("webTmpl")
        ))
    })

  val webAppSettings = basicSettings ++
    closureSettingsBuild ++
    revolverSettingsBuild ++
    assemblySettingsBuild ++
    liveScriptSettingsBuild ++
    lessSettingsBuild ++
    scalateSettingsBuild
}
