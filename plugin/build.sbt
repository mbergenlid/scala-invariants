
name := "plugin"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
    "mbergenlid.tools" %% "annotations" % "0.1-SNAPSHOT",
    "org.scala-lang" % "scala-compiler" % "2.10.3",
    "org.scalatest" % "scalatest_2.10" % "2.1.5" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

//scalacOptions in (Compile, console) <+=  (packageBin in Compile) map { bin =>
//    "-Xplugin:"+bin.absolutePath
//}

concurrentRestrictions in Global := {
  Tags.limitAll(1) :: Nil
}

