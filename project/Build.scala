import sbt._
import Keys._

object Build extends Build {
  lazy val root = Project(id = "invariants",
                          base = file("."),
                          settings = testSettings) aggregate(annotations, plugin, test)

  lazy val api = Project(id = "api",
                 base = file("api"))

  lazy val annotations = Project(id = "annotations",
                         base = file("annotations"))

  lazy val plugin = Project(id = "plugin",
                         base = file("plugin"))

  lazy val test = Project(id = "test",
                          base = file("test"))

  lazy val extractJarsTarget = SettingKey[File]("extract-jars-target", "Target directory for extracted JAR files")

  lazy val extractJars = TaskKey[Unit]("extract-jars", "Extracts JAR files")

  lazy val testSettings = Defaults.defaultSettings ++ Seq(

    // define the target directory
    extractJarsTarget <<= (target)(_ / "scala-2.10/classes"),

    // task to extract jar files
    extractJars <<= (packageBin in Compile in annotations, packageBin in Compile in plugin, extractJarsTarget) map { (bin1, bin2, dir) =>
      println("Running extract jars")
      println(s"${bin1.absolutePath} ${bin2.absolutePath}")
      IO.unzip(bin1, dir, {n: String => !n.startsWith("META-INF")})
      IO.unzip(bin2, dir, {n: String => !n.startsWith("META-INF")})
    },

    packageBin in Compile <<= (packageBin in Compile) dependsOn extractJars
  )
}
