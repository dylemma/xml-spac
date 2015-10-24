import sbt._
import Keys._

object XmlStreamBuild extends Build {


	lazy val commonSettings = Seq(
		version := "0.1-SNAPSHOT",
		crossScalaVersions := Seq("2.10.5", "2.11.6"),
		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
	)

	lazy val core = (project in file("core")) //Project("xml-stream", file("core"))
		.settings(commonSettings: _*)
		.settings(
			libraryDependencies ++= Seq(
				"com.typesafe.play" %% "play-iteratees" % "2.4.3",
				"com.typesafe.play" %% "play-functional" % "2.4.3"
			)
		)

	lazy val examples = (project in file("examples"))
		.settings(commonSettings: _*)
		.dependsOn(core)

	lazy val root = (project in file(".")).aggregate(core, examples)
}