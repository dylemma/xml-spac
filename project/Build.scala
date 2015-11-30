import scala.util.control.NonFatal

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
		.settings(apiDocSettings: _*)

	lazy val examples = (project in file("examples"))
		.settings(commonSettings: _*)
		.dependsOn(core)

	lazy val root = (project in file(".")).aggregate(core, examples)

	lazy val snapshotBranch = {
		try {
			val branch = "git rev-parse --abbrev-ref HEAD".!!.trim
			if(branch == "HEAD"){
				"git rev-parse HEAD".!!.trim
			} else branch
		} catch {
			case NonFatal(_) => "unknown"
		}
	}

	lazy val apiDocSettings = Seq(
		scalacOptions in (Compile, doc) <++= Def.task {
			val version = Keys.version.value
			val sourcePath = (baseDirectory in ThisBuild).value.getAbsolutePath
			val sourceTree =
				if(version endsWith "-SNAPSHOT") snapshotBranch
				else version
			val sourceUrl = "https://github.com/dylemma/xml-stream/tree/" + sourceTree + "\u20ac{FILE_PATH}.scala"
			Seq(
				"-sourcepath", sourcePath,
				"-doc-source-url", sourceUrl
			)
		}
	)
}