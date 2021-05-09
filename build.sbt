ThisBuild / organization := "io.dylemma"
ThisBuild / version := "0.9"
ThisBuild / scalaVersion := "2.13.0"
ThisBuild / crossScalaVersions := Seq("2.12.10", "2.13.5")

lazy val commonSettings = Seq(
	ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:higherKinds"),
	ThisBuild / scalacOptions ++= (scalaBinaryVersion.value match {
		case "2.12" => Seq("-Ypartial-unification")
		case _ => Nil
	}),
	addCompilerPlugin("org.typelevel" % "kind-projector" % "0.11.3" cross CrossVersion.full)
)

lazy val catsCore = "org.typelevel" %% "cats-core" % "2.6.0"
lazy val catsEffect = "org.typelevel" %% "cats-effect" % "3.1.0"
lazy val fs2Core = "co.fs2" %% "fs2-core" % "3.0.2"
lazy val fs2DataJson = "org.gnieh" %% "fs2-data-json" % "1.0.0-RC2"
lazy val fs2DataXml = "org.gnieh" %% "fs2-data-xml" % "1.0.0-RC2"
lazy val jacksonCore = "com.fasterxml.jackson.core" % "jackson-core" % "2.12.3"
lazy val typeName = "org.tpolecat" %% "typename" % "0.1.7"

lazy val testSettings = Seq(
	libraryDependencies ++= Seq(
		"org.scalatest" %% "scalatest" % "3.2.6" % Test,
		"org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
		"org.scalatestplus" %% "scalacheck-1-15" % "3.2.6.0" % Test,
	)
)

lazy val core = (project in file("core"))
	.settings(name := "spac-core")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(libraryDependencies ++= Seq(catsCore, catsEffect, fs2Core, typeName))

lazy val xml = (project in file("xml"))
	.settings(name := "xml-spac")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(core % "compile->compile;test->test")

lazy val xmlJavax = (project in file("xml-javax"))
	.settings(name := "xml-spac-javax")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(xml % "compile->compile;test->test")

lazy val xmlFs2Data = (project in file("xml-fs2-data"))
	.settings(name := "xml-spac-fs2-data")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(libraryDependencies += fs2DataXml)
	.dependsOn(xml % "compile->compile;test->test")

lazy val json = (project in file("json"))
	.settings(name := "json-spac")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(core % "compile->compile;test->test")

lazy val jsonJackson = (project in file("json-jackson"))
	.settings(name := "json-spac-jackson")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(libraryDependencies += jacksonCore)
	.dependsOn(json % "compile->compile;test->test")

lazy val jsonFs2Data = (project in file("json-fs2-data"))
	.settings(name := "json-spac-fs2-data")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(libraryDependencies += fs2DataJson)
	.dependsOn(json % "compile->compile;test->test")

lazy val examples = (project in file("examples"))
	.settings(commonSettings: _*)
	.settings(
		publish := {},
		libraryDependencies ++= Seq(catsEffect, fs2Core),
	)
	.dependsOn(core, xml, xmlJavax, xmlFs2Data)

lazy val root = (project in file("."))
	.aggregate(
		core,
		examples,
		xml, xmlFs2Data, xmlJavax,
		json, jsonJackson
	)
	.settings(commonSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(
		publish := {},
		publishArtifact := false,
		scalacOptions in (ScalaUnidoc, unidoc) += "-Ymacro-expand:none",
		unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(examples)
	)
	.enablePlugins(ScalaUnidocPlugin)


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
			"-groups",
			"-implicits",
			s"-implicits-hide:${classesForHiddenConversions.mkString(",")}",
			"-sourcepath", sourcePath,
			"-doc-source-url", sourceUrl
		)
	}
)

lazy val classesForHiddenConversions = Seq(
	// these end up being added to literally every class,
	// despite the fact that they should never actually be applied to a spac class
	"io.dylemma.spac.SourceToPullable",
	"io.dylemma.spac.xml.elem",

	// for some reason, specifying any `-implicits-hide` flag to the scaladoc task
	// causes it to *add* all the conversions from scala.Predef,
	// so I have to manually exclude those if I want to exclude any of my own classes
	"scala.Predef",
	"scala.Predef.any2stringadd",
	"scala.Predef.Ensuring",
	"scala.Predef.StringFormat",
	"scala.Predef.ArrowAssoc"
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