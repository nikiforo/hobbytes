val commonSettings = Seq(
  organization := "io.github.nikiforo",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.13.10"
)

val circeVersion = "0.14.5"

val root = (project in file("."))
  .settings(
    name := "hobbytes",
    commonSettings,
    addCompilerPlugin("com.olegpy" % "better-monadic-for_2.13" % "0.3.1"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "co.fs2" %% "fs2-io" % "3.6.1",
      "org.scalatest" %% "scalatest" % "3.2.16" % "test",
      "io.circe" %% "circe-core" % circeVersion % Test,
      "io.circe" %% "circe-parser" % circeVersion % Test
    )
  )
