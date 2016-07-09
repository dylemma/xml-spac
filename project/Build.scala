import scala.util.control.NonFatal

import sbt._
import Keys._
import spray.boilerplate.BoilerplatePlugin

object XmlStreamBuild extends Build {

	lazy val commonSettings = Seq(
		version := "0.2",
		scalaVersion := "2.11.8",
		scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
		organization := "io.dylemma"
	)

	lazy val core = (project in file("core"))
		.settings(name := "spac")
		.settings(commonSettings: _*)
		.settings(libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test")
		.enablePlugins(BoilerplatePlugin)
		.settings(apiDocSettings: _*)
		.settings(publishingSettings: _*)

	lazy val examples = (project in file("examples"))
		.settings(commonSettings: _*)
		.settings(
			publish := {},
			libraryDependencies += "joda-time" % "joda-time" % "2.9.4"
		)
		.dependsOn(core)

	lazy val root = (project in file("."))
		.settings(publish := {})
		.aggregate(core, examples)

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

	lazy val publishingSettings = Seq(
		publishMavenStyle := true,
		publishTo := {
			val nexus = "https://oss.sonatype.org/"
			if(isSnapshot.value)
				Some("snapshots" at nexus + "content/repositories/snapshots")
			else
				Some("releases" at nexus + "service/local/staging/deploy/maven2")
		},
		publishArtifact in Test := false,
		pomIncludeRepository := { _ => false },
		pomExtra := (
			<url>https://github.com/dylemma/xml-stream</url>
			<licenses>
				<license>
					<name>MIT</name>
					<url>http://opensource.org/licenses/MIT</url>
					<distribution>repo</distribution>
				</license>
			</licenses>
			<scm>
				<url>git@github.com:dylemma/xml-stream.git</url>
				<connection>scm:git:git@github.com:dylemma/xml-stream.git</connection>
			</scm>
			<developers>
				<developer>
					<id>dylemma</id>
					<name>Dylan Halperin</name>
					<url>http://dylemma.io/</url>
				</developer>
			</developers>
		)
	)
}