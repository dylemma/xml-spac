name := "xml-stream2"

version := "1.0"

scalaVersion := "2.11.6"

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")

libraryDependencies ++= Seq(
	"com.typesafe.play" %% "play-iteratees" % "2.4.3",
	"com.typesafe.play" %% "play-functional" % "2.4.3"
)
