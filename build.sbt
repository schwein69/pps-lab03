val scala3Version = "3.3.5"

lazy val root = project
  .in(file("."))
  .settings(
    name := "pps-lab03",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "com.github.sbt" % "junit-interface" % "0.13.2" % Test
  )
