organization := "mbergenlid.tools"

name := "annotations"

version := "0.1-SNAPSHOT"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

