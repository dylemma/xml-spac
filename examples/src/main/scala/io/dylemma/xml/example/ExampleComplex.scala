package io.dylemma.xml.example

import io.dylemma.xml.XMLEventEnumerator
import io.dylemma.xml.iteratee.ParsingDSL._
import play.api.libs.iteratee.{ Execution, Iteratee }

/**
 * Created by dylan on 10/11/2015.
 */
object ExampleComplex {

	case class LocRange(start: String, end: Option[String])
	implicit val LocRangeParser: Parser[LocRange] = (
		(* % "start") ~
		(* %? "end")
	)(LocRange.apply _)

	case class Location(path: String, line: Option[LocRange], col: Option[LocRange])
	implicit val LocationParser: Parser[Location] = (
		(* % "path") ~
		(* / "line").asOptional[LocRange] ~
		(* / "column").asOptional[LocRange]
	)(Location.apply _)

	case class Comment(user: String, body: String)
	implicit val CommentParser: Parser[Comment] = (
		(* % "user") ~
		(* % Text)
	)(Comment.apply _)

	case class Finding(severity: String, status: String, loc: Location, comments: List[Comment])
	implicit val FindingParser: Parser[Finding] = (
		(* % "severity") ~
		(* % "status") ~
		(* / "location").as[Location] ~
		(* / "comments" / "comment").asList[Comment]
	)(Finding.apply _)

	def main (args: Array[String]) {
		implicit val context = Execution.trampoline
		val parser = (Root / "findings" / "finding").foreach[Finding](println)
		val xmlSource = XMLEventEnumerator{ () =>
			getClass.getResourceAsStream("/example-complex.xml")
		}
		xmlSource |>>> parser.toIteratee
	}
}
