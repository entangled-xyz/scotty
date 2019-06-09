import xerial.sbt.Sonatype._
import ReleaseTransformations._

ThisBuild / name := "scotty"
ThisBuild / organization := "xyz.entangled"
ThisBuild / scalaVersion := "2.12.8"
ThisBuild / crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.8")

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5" % "test"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

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