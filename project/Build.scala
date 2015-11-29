import sbt._
import Keys._
import spray.boilerplate.BoilerplatePlugin.Boilerplate

object XmlStreamBuild extends Build {

	lazy val commonSettings = Seq(
		version := "0.1-SNAPSHOT",
		crossScalaVersions := Seq("2.10.5", "2.11.6"),
		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
	)

	lazy val core = (project in file("core"))
		.settings(commonSettings: _*)
		.settings(libraryDependencies += "com.typesafe.play" %% "play-iteratees" % "2.4.3")
		.settings(libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test")
		.settings(Boilerplate.settings: _*)

	lazy val examples = (project in file("examples"))
		.settings(commonSettings: _*)
		.dependsOn(core)

	lazy val root = (project in file(".")).aggregate(core, examples)
}