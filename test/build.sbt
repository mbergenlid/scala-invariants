organization := "mbergenlid.tools" 

name := "test"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
    "mbergenlid.tools" %% "annotations" % "0.1-SNAPSHOT",
    "bounded-integers" %% "bounded-integers" % "0.1-SNAPSHO",
    "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

concurrentRestrictions in Global := {
  Tags.limitAll(1) :: Nil
}
