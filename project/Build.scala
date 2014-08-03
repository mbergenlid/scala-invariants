import sbt._
import Keys._

object Build extends Build {
  lazy val root = Project(id = "invariants",
                          base = file("."),
                          settings = testSettings) aggregate(annotations, plugin, api, testProject)

  lazy val annotations = Project(id = "annotations",
    base = file("annotations"))

  lazy val api = Project(id = "api",
                 base = file("api")).dependsOn(annotations)

  lazy val plugin = Project(id = "plugin",
                         base = file("plugin")).dependsOn(api, annotations)

  lazy val testProject = Project(id = "test",
                          base = file("test")).dependsOn(plugin, annotations, api)

  lazy val extractJarsTarget = SettingKey[File]("extract-jars-target", "Target directory for extracted JAR files")

  lazy val extractJars = TaskKey[Unit]("extract-jars", "Extracts JAR files")

  lazy val testSettings = Defaults.defaultSettings ++ Seq[Setting[_]](

    // define the target directory
    extractJarsTarget <<= (classDirectory in Compile in plugin),

    // task to extract jar files
    extractJars <<= (packageBin in Compile in annotations,
        packageBin in Compile in api,
        extractJarsTarget, streams) map { (bin1, bin2, dir, s) =>

      s.log.info(s"Extracting ${bin1.absolutePath}")
      IO.unzip(bin1, dir, {n: String => !n.startsWith("META-INF")})
      s.log.info(s"Extracting ${bin2.absolutePath}")
      IO.unzip(bin2, dir, {n: String => !n.startsWith("META-INF")})
    },

    packageBin in Compile in plugin <<= (packageBin in Compile in plugin) dependsOn extractJars,

    (test in Test in testProject) <<= (test in Test in testProject) dependsOn (packageBin in Compile in plugin),

    (testOnly in Test in testProject) <<= (testOnly in Test in testProject) dependsOn (packageBin in Compile in plugin),

    testOptions in Test in testProject +=
      Tests.Setup { () =>
        System.setProperty("PLUGIN",
          (artifactPath in packageBin in Compile in plugin).value.getAbsolutePath)
        System.setProperty("TEST_OUTPUT",
          (classDirectory in Compile in testProject).value.getAbsolutePath)
      }
  )
}
