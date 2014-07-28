organization := "mbergenlid.tools" 

name := "test"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.4"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)


libraryDependencies ++= Seq(
    "mbergenlid.tools" %% "annotations" % "0.1-SNAPSHOT",
    "default" %% "plugin" % "0.1-SNAPSHOT",
    "org.scalatest" % "scalatest_2.10" % "2.0" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

testOptions in Test += Tests.Setup ( () => System.setProperty("SCALA_HOME", System.getenv("SCALA_HOME")) )

testOptions in Test += Tests.Setup ( () => System.setProperty("PLUGIN_ROOT", "../") )


//scalacOptions in Compile <+= (packageBin in Compile) map { bin =>
//  "-Xplugin:"+bin.absolutePath
//}

concurrentRestrictions in Global := {
  Tags.limitAll(1) :: Nil
}
