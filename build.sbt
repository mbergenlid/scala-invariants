name := "bounded-integers"

version := "0.1-SNAPSHOT"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
    "org.scala-lang" % "scala-compiler" % "2.10.3",
    "org.scalatest" % "scalatest_2.10" % "2.0" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation")

scalacOptions in (Compile, console) <+= (packageBin in Compile) map { bin =>
    "-Xplugin:"+bin.absolutePath
}
