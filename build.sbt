ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val utilsLib = RootProject(file("../scalaAwtDrawing"))

lazy val root = (project in file("."))
  .settings(
    name := "rraayyss"
  ).dependsOn(utilsLib)
