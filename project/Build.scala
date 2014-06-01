import sbt._
import sbt.Keys._
import BuildSettings._
import sbtassembly.Plugin._

//TODO: Add support for renaming files before copying
//TODO: Add support for modify files before going to jar. Change files to production mode
object Server extends Build with WithDependencies with WithResolvers {
  val serverDist = TaskKey[File]("server-dist", "Creates a distributable zip file containing the publet standalone server.")
  lazy val serverDistImpl = Def.task {
    val log = streams.value.log
    val name = Keys.name.value
    val version = Keys.version.value
    val crossTarget = Keys.crossTarget.value
    val target = Keys.target.value
    val pkgBin = Keys.packageBin.in(Compile).value
    val resources = Keys.managedResources.in(Compile).value
    val resourceDirectory = Keys.resourceDirectory.in(Compile).value
    val dependenciesF = Keys.dependencyClasspath.in(Compile).value
    val dependencies = dependenciesF.map(_.data)
    val distDir = target / (name + "-" + version)
    val zipFile = target / (distDir.base + ".zip")
    def copyFile(orig: File, relativeFile: File) = (destDir: File) => {
      log.info("Copying file " + orig.getName + " -> " + (destDir / relativeFile.toString).toString)
      IO.copyFile(orig, destDir / relativeFile.toString)
    }
    log.info(dependencies.mkString(":"))
    val mapFiles = (func:(File => Option[File])) => (files: Seq[File]) =>
      files.map(file => func(file).map(copyFile(file,_)))
    val relativize = (relativeToPath: File) => mapFiles(_.relativeTo(relativeToPath))
    dependencies.foreach {f => log.info(f.toString)}
    val addFiles = mapFiles(f => Some(f))
    val filterByName = (names: Seq[File]) => mapFiles(f => if(names.contains(f)) Some(f) else None)
    val distTree = Map(
      "bin" -> (relativize(crossTarget)(Seq(pkgBin)) ++
        relativize(resourceDirectory)(Seq(resourceDirectory / "start.sh"))),
      "var/log" -> Seq(),
      "etc" -> relativize(resourceDirectory)(Seq(
        resourceDirectory / "avsl-production.conf",
        resourceDirectory / "application.conf")),
      "lib" -> mapFiles(f => Some(file(f.getName)))(dependencies)
    )
    IO.delete(zipFile)
    IO.delete(distDir)
    distTree.foreach {
      case (dirName, fileFuncs) => {
        log.info("Creating " + (distDir / dirName).toString)
        IO.createDirectory(distDir / dirName)
        fileFuncs.filter(_.isDefined).foreach(_.get(distDir / dirName))
      }
    }
    def entries(f: File): List[File] =
      f :: (
        if (f.isDirectory) IO.listFiles(f).toList.flatMap(entries(_))
        else Nil
      )
    IO.zip((entries(distDir)).
      map(d => (d, d.getAbsolutePath.substring(distDir.getParent.length))), zipFile)
    zipFile
  }
  lazy val module = Project("AppTest", file(".")).
    aggregate(unfilteredMCore, unfilteredMMail, unfilteredMBlog, unfilteredMCaptcha).
    dependsOn(unfilteredMCore, unfilteredMMail, unfilteredMBlog, unfilteredMCaptcha).
    settings(webAppSettings: _*).
    settings(libraryDependencies ++= dependenciesBuild).
    settings(resolvers ++= resolversBuild).
    settings(serverDist := serverDistImpl.value).
    settings(packageOptions in (Compile, packageBin) +=
    Package.ManifestAttributes(
      java.util.jar.Attributes.Name.CLASS_PATH -> Keys.dependencyClasspath.in(Compile).value.map(_.data).mkString(" "))
    ).in(file("."))

  lazy val unfilteredMCore = project.
    settings(UnfilteredMCoreSettings.settings: _*).
    settings(libraryDependencies ++= UnfilteredMCoreSettings.dependenciesBuild)
  lazy val unfilteredMMail = project.
    settings(UnfilteredMMailSettings.settings: _*).
    settings(libraryDependencies ++= UnfilteredMMailSettings.dependenciesBuild).
    dependsOn(unfilteredMCore)
  lazy val unfilteredMCaptcha = project.
    settings(UnfilteredMCaptchaSettings.settings: _*).
    settings(libraryDependencies ++= UnfilteredMCaptchaSettings.dependenciesBuild).
    dependsOn(unfilteredMCore)
  lazy val unfilteredMBlog = project.
    settings(UnfilteredMBlogSettings.settings: _*).
    settings(libraryDependencies ++= UnfilteredMBlogSettings.dependenciesBuild).
    dependsOn(unfilteredMCore)


  //lazy val unfilteredM-core = project.settings()
    /*settings(packageOptions in (Compile, packageBin) +=  {
      import java.util.jar.{Attributes, Manifest}
      val manifest = new Manifest
      manifest.getAttributes("foo/bar/").put(Attributes.Name.SEALED, "false")
      Package.JarManifest( manifest )
    })*/
}
