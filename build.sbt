import xerial.sbt.Sonatype._
import ReleaseTransformations._

val defaultScalaVersion = "2.13.0"

ThisBuild / name := "scotty"
ThisBuild / organization := "xyz.entangled"
ThisBuild / scalaVersion := defaultScalaVersion
ThisBuild / crossScalaVersions := Seq(
  defaultScalaVersion,
  "2.11.12",
  "2.12.8",
  )

val scalaTestVersion = "3.0.8"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, major)) if major >= 13 => Seq("org.scala-lang.modules" %% "scala-parallel-collections" % "0.2.0")
    case _ => Seq()
  }
}

libraryDependencies += "org.scalactic" %% "scalactic" % scalaTestVersion % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test"

publishTo := sonatypePublishTo.value

publishMavenStyle := true

licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

sonatypeProjectHosting := Some(GitHubHosting("entangled-xyz", "scotty", "vasinov@me.com"))

developers := List(
  Developer(id="vasinov", name="Vasily Vasinov", email="vasinov@me.com", url=url("https://www.vasinov.com"))
)

releaseCrossBuild := true
releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  releaseStepCommandAndRemaining("+publishSigned"),
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeReleaseAll"),
  pushChanges
)