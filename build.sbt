lazy val commonSettings = Seq(
	version := "0.8",
	scalaVersion := "2.13.0",
	crossScalaVersions := Seq("2.12.10", "2.13.5"),
	scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:higherKinds"),
	scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.12" => Seq("-Ypartial-unification")
		case _ => Nil
	}),
	organization := "io.dylemma",
	addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full),
)

lazy val catsCore = "org.typelevel" %% "cats-core" % "2.1.0"
lazy val catsEffect = "org.typelevel" %% "cats-effect" % "2.1.0"
lazy val fs2Core = "co.fs2" %% "fs2-core" % "2.2.1"
lazy val jacksonCore = "com.fasterxml.jackson.core" % "jackson-core" % "2.10.0"
lazy val jodaTime = "joda-time" % "joda-time" % "2.9.4"

lazy val testSettings = Seq(
	libraryDependencies ++= Seq(
		"org.scalatest" %% "scalatest" % "3.2.6" % Test,
		"org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
		"org.scalatestplus" %% "scalacheck-1-15" % "3.2.6.0" % Test,

	)
)

lazy val core = (project in file("core"))
	.enablePlugins(spray.boilerplate.BoilerplatePlugin)
	.settings(name := "spac-core")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(
		libraryDependencies += catsCore,
		libraryDependencies += catsEffect, // maybe only necessary for representing streams?
	)

lazy val coreFs2 = (project in file("core-fs2"))
	.settings(name := "spac-fs2")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(core)
   .settings(
	   libraryDependencies += fs2Core
   )

lazy val xml = (project in file("xml"))
	.settings(name := "xml-spac")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(core)

lazy val xmlJavax = (project in file("xml-javax"))
	.settings(name := "xml-spac-javax")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(xml)

lazy val json = (project in file("json"))
	.settings(name := "json-spac")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(
		libraryDependencies += jacksonCore
	)
	.dependsOn(core)

lazy val examples = (project in file("examples"))
	.settings(commonSettings: _*)
	.settings(
		publish := {},
		libraryDependencies ++= Seq(catsEffect, fs2Core, jodaTime),
		scalacOptions += "-Xlog-implicits"
	)
	.dependsOn(core, coreFs2, xml, xmlJavax)

lazy val root = (project in file("."))
	.aggregate(core, xml, json, xmlJavax)
	.settings(
		publish := {},
		publishArtifact := false
	)

lazy val snapshotBranch = {
	import scala.util.control.NonFatal
	import scala.sys.process._
	try {
		val branch = "git rev-parse --abbrev-ref HEAD".!!.trim
		if (branch == "HEAD") {
			"git rev-parse HEAD".!!.trim
		} else branch
	} catch {
		case NonFatal(_) => "unknown"
	}
}

lazy val apiDocSettings = Seq(
	scalacOptions in(Compile, doc) ++= {
		val version = Keys.version.value
		val sourcePath = (baseDirectory in ThisBuild).value.getAbsolutePath
		val sourceTree =
			if (version endsWith "-SNAPSHOT") snapshotBranch
			else version
		val sourceUrl = "https://github.com/dylemma/xml-spac/tree/" + sourceTree + "\u20ac{FILE_PATH}.scala"
		Seq(
			"-implicits",
			"-sourcepath", sourcePath,
			"-doc-source-url", sourceUrl
		)
	}
)

lazy val publishingSettings = Seq(
	publishMavenStyle := true,
	publishTo := {
		val nexus = "https://oss.sonatype.org/"
		if (isSnapshot.value)
			Some("snapshots" at nexus + "content/repositories/snapshots")
		else
			Some("releases" at nexus + "service/local/staging/deploy/maven2")
	},
	publishArtifact in Test := false,
	pomIncludeRepository := { _ => false },
	pomExtra := (
		<url>https://github.com/dylemma/xml-spac</url>
			<licenses>
				<license>
					<name>MIT</name>
					<url>http://opensource.org/licenses/MIT</url>
					<distribution>repo</distribution>
				</license>
			</licenses>
			<scm>
				<url>git@github.com:dylemma/xml-spac.git</url>
				<connection>scm:git:git@github.com:dylemma/xml-spac.git</connection>
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