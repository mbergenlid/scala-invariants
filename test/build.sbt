organization := "mbergenlid.tools" 

name := "test"

version := "0.1-SNAPSHOT"

resolvers ++= Seq(
  "Sonatype Releases" at "http://oss.sonatype.org/content/repositories/releases"
)


libraryDependencies ++= Seq(
    "org.scalatest" %% "scalatest" % "2.1.5" % "test",
    "org.scalacheck" %% "scalacheck" % "1.11.3" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

//TaskKey[Unit]("dep") <<= (fullClasspath in Compile).map(cp => println(cp))
//scalacOptions in Compile <+= (packageBin in Compile) map { bin =>
//  "-Xplugin:"+bin.absolutePath
//}

concurrentRestrictions in Global := {
  Tags.limitAll(1) :: Nil
}
