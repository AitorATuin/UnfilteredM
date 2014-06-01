import sbt._
import sbt.Keys._

object UnfilteredMMailSettings extends WithResolvers {
  val nameM = "unfilteredM-mail"
  val versionM = "0.2-SNAPSHOT"
  val scalaVersionM = "2.10.2"
  val organizationM = "com.logikujo"
  val dependenciesBuild = Seq(
    "org.clapper" % "avsl_2.10" % "1.0.1",
    "com.github.kxbmap" %% "configs" % "0.2.1",
    "org.apache.commons" % "commons-email" % "1.3.2",
    "org.scalaz" %% "scalaz-core" % "7.0.6"

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
