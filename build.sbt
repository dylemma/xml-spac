lazy val commonSettings = Seq(
	version := "0.6",
	scalaVersion := "2.13.0",
	crossScalaVersions := Seq("2.10.6", "2.11.11", "2.12.2", "2.13.0"),
	scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature"),
	organization := "io.dylemma"
)

lazy val testSettings = Seq(
	libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

lazy val core = (project in file("core"))
	.enablePlugins(spray.boilerplate.BoilerplatePlugin)
	.settings(name := "spac-core")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)

lazy val xml = (project in file("xml"))
	.settings(name := "xml-spac")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.dependsOn(core)

lazy val json = (project in file("json"))
	.settings(name := "json-spac")
	.settings(commonSettings: _*)
	.settings(testSettings: _*)
	.settings(apiDocSettings: _*)
	.settings(publishingSettings: _*)
	.settings(
		libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "latest.integration" % "provided",
		libraryDependencies += "com.fasterxml.jackson.core" % "jackson-core" % "latest.integration" % "test"
	)
	.dependsOn(core)

lazy val examples = (project in file("examples"))
	.settings(commonSettings: _*)
	.settings(
		publish := {},
		libraryDependencies += "joda-time" % "joda-time" % "2.9.4"
	)
	.dependsOn(core, xml)

lazy val root = (project in file("."))
	.aggregate(core, xml, json)
	.settings(
		publish := {},
		publishArtifact := false
	)

lazy val snapshotBranch = {
	import scala.util.control.NonFatal
	import scala.sys.process._
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
	scalacOptions in (Compile, doc) ++= {
		val version = Keys.version.value
		val sourcePath = (baseDirectory in ThisBuild).value.getAbsolutePath
		val sourceTree =
			if(version endsWith "-SNAPSHOT") snapshotBranch
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
		if(isSnapshot.value)
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