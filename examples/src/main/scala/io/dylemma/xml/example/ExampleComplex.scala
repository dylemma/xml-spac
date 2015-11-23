package io.dylemma.xml.example

import io.dylemma.xml.Parser
import io.dylemma.xml.ParsingDSL._
import play.api.libs.iteratee.Execution.Implicits.trampoline

/**
 * Created by dylan on 10/11/2015.
 */
object ExampleComplex {

	case class LocRange(start: String, end: Option[String])
	implicit val LocRangeParser: Parser[LocRange] = (
		(* % "start") &
		(* %? "end")
	).join(LocRange)

	case class Location(path: String, line: Option[LocRange], col: Option[LocRange])
	implicit val LocationParser: Parser[Location] = (
		(* % "path") &
		(* / "line").asOptional[LocRange] &
		(* / "column").asOptional[LocRange]
	).join(Location)

	case class Comment(user: String, body: String)
	implicit val CommentParser: Parser[Comment] = (
		(* % "user") &
		(* % Text)
	).join(Comment)

	case class Finding(severity: String, status: String, loc: Location, comments: List[Comment])
	implicit val FindingParser: Parser[Finding] = (
		(* % "severity") &
		(* % "status") &
		(* / "location").as[Location] &
		(* / "comments" / "comment").asList[Comment]
	).join(Finding)

	def main (args: Array[String]) {
		val parser = (Root / "findings" / "finding").foreach[Finding](println)
		parser parse getClass.getResourceAsStream("/example-complex.xml")
	}
}
