val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "hkd",
    version := "0.1.0",
    scalacOptions ++= Seq("-Ykind-projector"),

    scalaVersion := scala3Version,
  )
