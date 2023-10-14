Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / organization := "io.dylemma"
ThisBuild / version := "0.12.1"
ThisBuild / versionScheme := Some("early-semver")
ThisBuild / scalaVersion := "2.13.12"
ThisBuild / crossScalaVersions := Seq("2.12.18", "2.13.12", "3.3.1")
ThisBuild / scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-language:higherKinds")
ThisBuild / scalacOptions ++= (scalaBinaryVersion.value match {
	case "2.12" => Seq("-Ypartial-unification")
	case _ => Nil
})

lazy val catsCore = "org.typelevel" %% "cats-core" % "2.10.0"
lazy val catsEffect = "org.typelevel" %% "cats-effect" % "3.5.2"
lazy val fs2Core = "co.fs2" %% "fs2-core" % "3.9.2"
lazy val fs2Io = "co.fs2" %% "fs2-io" % "3.9.2"
lazy val fs2DataJson = "org.gnieh" %% "fs2-data-json" % "1.9.0"
lazy val fs2DataXml = "org.gnieh" %% "fs2-data-xml" % "1.9.0"
lazy val jacksonCore = "com.fasterxml.jackson.core" % "jackson-core" % "2.15.3"
lazy val typeName = "org.tpolecat" %% "typename" % "1.1.0"

lazy val testSettings = Seq(
	libraryDependencies ++= Seq(
		"org.scalatest" %% "scalatest" % "3.2.17" % Test,
		"org.scalacheck" %% "scalacheck" % "1.17.0" % Test,
		"org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test,
	)
)

lazy val core = (project in file("core"))
	.settings(name := "spac-core")
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(libraryDependencies ++= Seq(catsCore, typeName))

lazy val coreFs2 = (project in file("core-fs2"))
	.settings(name := "spac-interop-fs2")
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(libraryDependencies ++= Seq(catsCore, catsEffect, fs2Core))
	.dependsOn(core % "compile->compile;test->test")

lazy val xml = (project in file("xml"))
	.settings(name := "xml-spac")
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(core % "compile->compile;test->test")
	.dependsOn(coreFs2 % "test->test")
	.settings(libraryDependencies ++= Seq(catsEffect, fs2Core).map(_ % Test))

lazy val xmlJavax = (project in file("xml-javax"))
	.settings(name := "xml-spac-javax")
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(xml % "compile->compile;test->test")

lazy val xmlFs2Data = (project in file("xml-fs2-data"))
	.settings(name := "xml-spac-fs2-data")
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(libraryDependencies += fs2DataXml)
	.dependsOn(xml % "compile->compile;test->test")

lazy val json = (project in file("json"))
	.settings(name := "json-spac")
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(coreFs2 % "test->test")
	.dependsOn(core % "compile->compile;test->test")

lazy val jsonJackson = (project in file("json-jackson"))
	.settings(name := "json-spac-jackson")
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(libraryDependencies += jacksonCore)
	.dependsOn(json % "compile->compile;test->test")

lazy val jsonFs2Data = (project in file("json-fs2-data"))
	.settings(name := "json-spac-fs2-data")
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(libraryDependencies += fs2DataJson)
	.dependsOn(coreFs2)
	.dependsOn(json % "compile->compile;test->test")

lazy val examples = (project in file("examples"))
	.settings(
		publish := {},
		publish / skip := true,
		libraryDependencies ++= Seq(catsEffect, fs2Core, fs2Io),
	)
	.dependsOn(core, coreFs2, xml, xmlJavax, xmlFs2Data)

lazy val root = (project in file("."))
	.aggregate(
		core,
		coreFs2,
		examples,
		xml, xmlFs2Data, xmlJavax,
		json, jsonJackson, jsonFs2Data
	)
	.settings(apiDocSettings: _*)
	.settings(
		publish := {},
		publishArtifact := false,
		publish / skip := true,
		ScalaUnidoc / unidoc / scalacOptions += "-Ymacro-expand:none",
		ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(examples)
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
	Compile / doc / scalacOptions ++= {
		val version = Keys.version.value
		val sourcePath = (ThisBuild / baseDirectory).value.getAbsolutePath
		val sourceTree =
			if (version endsWith "-SNAPSHOT") snapshotBranch
			else version
		val sourceUrl = "https://github.com/dylemma/xml-spac/tree/" + sourceTree + "\u20ac{FILE_PATH}.scala"
		CrossVersion.partialVersion(scalaVersion.value) match {
			case Some((2, _)) =>
				Seq(
					"-groups",
					"-implicits",
					s"-implicits-hide:${ classesForHiddenConversions.mkString(",") }",
					"-skip-packages", scaladocIgnoredPackages.mkString(":"),
					"-sourcepath", sourcePath,
					"-doc-source-url", sourceUrl
				)
			case _ =>
				// ScalaDoc in Scala 3 seems like it's still working out some stuff.
				// Flags like `groups` are supposed to work according to their documentation, but I get "bad option -groups" when I use it.
				// For now I'm just going to leave off all of the options entirely and figure it out later.
				Nil
		}
	}
)

lazy val classesForHiddenConversions = Seq(
	// these end up being added to literally every class,
	// despite the fact that they should never actually be applied to a spac class
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
lazy val scaladocIgnoredPackages = Seq(
	"io.dylemma.spac.impl",
	"io.dylemma.spac.xml.impl",
	"io.dylemma.spac.json.impl",
	"io.dylemma.spac.interop.fs2.impl"
)

lazy val publishingSettings = Seq(
	publishMavenStyle := true,
	publishTo := sonatypePublishToBundle.value,
	Test / publishArtifact := false,
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