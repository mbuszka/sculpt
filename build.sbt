ThisBuild / scalaVersion := "2.12.10"
ThisBuild / organization := "mbuszka"

resolvers ++= Seq(
  Resolver.sonatypeRepo("snapshots"),
  Resolver.sonatypeRepo("releases")
)

lazy val sculpt = (project in file("."))
  .settings(
    name := "sculpt",
    libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2",
    libraryDependencies += "com.lihaoyi" %% "pprint" % "0.5.6",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0",
    libraryDependencies += "edu.berkeley.cs" %% "chisel3" % "3.2-SNAPSHOT",
    libraryDependencies += "edu.berkeley.cs" %% "chisel-testers2" % "0.1-SNAPSHOT",
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.1.0",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.1.0" % Test,

    scalacOptions += "-Xsource:2.11",
    scalacOptions += "-Ypartial-unification"
  )
