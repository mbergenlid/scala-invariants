organization := "mbergenlid.tools" 

name := "propertytests"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.4"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)


libraryDependencies ++= Seq(
    "mbergenlid.tools" %% "annotations" % "0.1-SNAPSHOT",
    "plugin" %% "plugin" % "0.1-SNAPSHOT",
    "org.scalatest" % "scalatest_2.10" % "2.0" % "propertytests",
    "org.scalacheck" %% "scalacheck" % "1.11.3"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

//scalacOptions in Compile <+= (packageBin in Compile) map { bin =>
//  "-Xplugin:"+bin.absolutePath
//}

concurrentRestrictions in Global := {
  Tags.limitAll(1) :: Nil
}
