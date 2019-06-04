import xerial.sbt.Sonatype._
import ReleaseTransformations._

inThisBuild(Seq(
  name := "scotty",
  organization := "xyz.entangled",
  scalaVersion := "2.12.8",
  crossScalaVersions := Seq("2.10.7", "2.11.12", "2.12.8")
))

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

publishTo := sonatypePublishTo.value

sonatypeProfileName := "xyz.entangled"

publishMavenStyle := true

licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

sonatypeProjectHosting := Some(GitHubHosting("entangled-xyz", "scotty", "des.elyon@gmail.com"))

developers := List(
  Developer(id="vasinov", name="Vasily Vasinov", email="des.elyon@gmail.com", url=url("https://www.vasinov.com"))
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