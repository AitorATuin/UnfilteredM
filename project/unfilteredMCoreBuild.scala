import sbt._
import sbt.Keys._

object UnfilteredMCoreSettings extends WithResolvers {
  val nameM = "unfilteredM-core"
  val versionM = "0.2-SNAPSHOT"
  val scalaVersionM = "2.10.4"
  val organizationM = "com.logikujo"
  val versions = Map(
    "unfiltered" -> "0.8.0"
  )
  val dependenciesBuild = Seq(
    "net.databinder" %% "unfiltered-filter" % versions("unfiltered"),
    //"net.databinder" %% "unfiltered-jetty" % versions("unfiltered"),
    "net.databinder" %% "unfiltered-filter-async" % versions("unfiltered"),
    "net.databinder" %% "unfiltered-directives" % versions("unfiltered"),
    "net.databinder" %% "unfiltered-specs2" % versions("unfiltered"),
    "net.databinder" %% "unfiltered-scalatest" % "0.7.1",
    // note: scalate 1.5.3 leaves sbt's run task hanging
    "org.fusesource.scalate" % "scalate-core_2.10" % "1.6.1",
    "org.clapper" % "avsl_2.10" % "1.0.1",
    "com.typesafe.akka" %% "akka-actor" % "2.3-M2",
    "com.github.kxbmap" %% "configs" % "0.2.1",
    "org.scalaz" %% "scalaz-core" % "7.0.6",
    "io.argonaut" %% "argonaut" % "6.0.1",
    "net.databinder.dispatch" %% "dispatch-core" % "0.11.0",
    "com.github.nscala-time" %% "nscala-time" % "0.8.0",
    "org.reactivemongo" %% "reactivemongo" % "0.11.0-SNAPSHOT",
    "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test",
    "org.scalamock" %% "scalamock-scalatest-support" % "3.0.1" % "test",
    "org.mockito" % "mockito-core" % "1.8.5" % "test"
  )
  val resolversM = Seq(
    "sonatype-public" at "https://oss.sonatype.org/content/groups/public",
    "Typesafe repository releases" at "http://repo.typesafe.com/typesafe/releases/"
  )
  val settings = Defaults.defaultSettings ++ Seq(
    name := nameM,
    version := versionM,
    scalaVersion := scalaVersionM,
    organization := organizationM,
    resolvers ++= resolversBuild,
    scalacOptions <<= scalaVersion map { sv: String =>
      if (sv.startsWith("2.10."))
        Seq("-deprecation", "-unchecked", "-feature", "-language:postfixOps", "-language:implicitConversions")
      else
        Seq("-deprecation", "-unchecked")
    }
  )
}
