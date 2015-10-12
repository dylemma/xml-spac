package io.dylemma.xml.example

import io.dylemma.xml.XMLEventEnumerator
import io.dylemma.xml.iteratee.ParsingDSL._
import play.api.libs.iteratee.{ Execution, Iteratee }

/**
 * Created by dylan on 10/11/2015.
 */
object ExampleComplex {

	case class LocRange(start: String, end: Option[String])
	implicit val LocRangeParser = (
		(Elem % "start") ~
		(Elem %? "end")
	)(LocRange.apply _)

	case class Location(path: String, line: Option[LocRange], col: Option[LocRange])
	implicit val LocationParser = (
		(Elem % "path") ~
		(Elem \ "line").asOptional[LocRange] ~
		(Elem \ "column").asOptional[LocRange]
	)(Location.apply _)

	case class Comment(user: String, body: String)
	implicit val CommentParser = (
		(Elem % "user") ~
		(Elem \ Text)
	)(Comment.apply _)

	case class Finding(severity: String, status: String, loc: Location, comments: List[Comment])
	implicit val FindingParser = (
		(Elem % "severity") ~
		(Elem % "status") ~
		(Elem \ "location").as[Location] ~
		(Elem \ "comments" \ "comment").asList[Comment]
	)(Finding.apply _)

	def main (args: Array[String]) {
		implicit val context = Execution.trampoline
		val handleFinding = Iteratee.foreach[Parser.Result[Finding]](println)
		val parser = (Root \ "findings" \ "finding").consumeAs[Finding](handleFinding)
		val xmlSource = XMLEventEnumerator{ () =>
			getClass.getResourceAsStream("/example-complex.xml")
		}
		xmlSource |>>> parser.toIteratee
	}
}
