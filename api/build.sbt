
name := "api"

organization := "mbergenlid.scalainvariants"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "mbergenlid.tools" %% "annotations" % "0.1-SNAPSHOT",
  "org.scala-lang" % "scala-compiler" % "2.10.3",
  "org.scalatest" % "scalatest_2.10" % "2.1.5" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")