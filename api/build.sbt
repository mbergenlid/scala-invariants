
name := "api"

organization := "mbergenlid.scalainvariants"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "mbergenlid.tools" %% "annotations" % "0.1-SNAPSHOT",
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "org.scalatest" %% "scalatest" % "2.1.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")