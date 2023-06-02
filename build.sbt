val commonSettings = Seq(
  organization := "me.nikiforo",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.13.10"
)

val circeVersion = "0.14.5"

val root = (project in file("."))
  .settings(
    name := "leetcode",
    commonSettings,
    addCompilerPlugin("com.olegpy" % "better-monadic-for_2.13" % "0.3.1"),
    libraryDependencies ++= Seq(
      "co.fs2" %% "fs2-io" % "3.6.1",
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
      "io.circe" %% "circe-core" % circeVersion % Test,
      "io.circe" %% "circe-parser" % circeVersion % Test
    )
  )
