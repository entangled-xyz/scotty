sonatypeProfileName := "xyz.entangled"

publishMavenStyle := true

licenses := Seq("APL2" -> url("http://www.apache.org/licenses/LICENSE-2.0.txt"))

import xerial.sbt.Sonatype._

sonatypeProjectHosting := Some(GitHubHosting("entangled-xyz", "scotty", "des.elyon@gmail.com"))

developers := List(
  Developer(id="vasinov", name="Vasily Vasinov", email="des.elyon@gmail.com", url=url("https://www.vasinov.com"))
)