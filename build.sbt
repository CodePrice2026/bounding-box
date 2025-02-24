ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
    name := "bounding-box"
  )
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % Test

assembly / mainClass := Some("exercise.BoundingBox")

assembly / assemblyJarName := s"${name.value}.jar"
