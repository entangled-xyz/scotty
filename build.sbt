name := "scotty"

version := "0.1.0"

scalaVersion := "2.12.8"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

useGpg := true

ThisBuild / organization := "xyz.entangled"
ThisBuild / organizationName := "entangled"
ThisBuild / organizationHomepage := Some(url("http://www.entangled.xyz"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/entangled-xyz/scotty"),
    "scm:git@github.com:entangled-xyz/scotty.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id    = "vasinov",
    name  = "Vasily Vasinov",
    email = "vasinov@me.com",
    url   = url("https://www.vasinov.com")
  )
)

ThisBuild / description := "Scotty is a quantum computing platform for Scala developers."
ThisBuild / licenses := List("Apache 2" -> new URL("http://www.apache.org/licenses/LICENSE-2.0.txt"))
ThisBuild / homepage := Some(url("https://github.com/entangled-xyz/scotty"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true