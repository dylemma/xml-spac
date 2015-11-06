package io.dylemma.xml.example

import io.dylemma.xml.XMLEventEnumerator
import io.dylemma.xml.iteratee.ParsingDSL._
import play.api.libs.iteratee.{ Execution, Iteratee }

/**
 * Created by dylan on 10/11/2015.
 */
object ExampleComplex {

	case class LocRange(start: String, end: Option[String])
	implicit val LocRangeParser: AnyContextParser[LocRange] = (
		(* % "start") &
		(* %? "end")
	).join(LocRange)

	case class Location(path: String, line: Option[LocRange], col: Option[LocRange])
	implicit val LocationParser: AnyContextParser[Location] = (
		(* % "path") &
		(* / "line").asOptional[LocRange] &
		(* / "column").asOptional[LocRange]
	).join(Location)

	case class Comment(user: String, body: String)
	implicit val CommentParser: AnyContextParser[Comment] = (
		(* % "user") &
		(* % Text)
	).join(Comment)

	case class Finding(severity: String, status: String, loc: Location, comments: List[Comment])
	implicit val FindingParser: AnyContextParser[Finding] = (
		(* % "severity") &
		(* % "status") &
		(* / "location").as[Location] &
		(* / "comments" / "comment").asList[Comment]
	).join(Finding)

	def main (args: Array[String]) {
		implicit val context = Execution.trampoline
		val parser = (Root / "findings" / "finding").foreach[Finding](println)
		val xmlSource = XMLEventEnumerator{ () =>
			getClass.getResourceAsStream("/example-complex.xml")
		}
		xmlSource |>>> parser.toIteratee()
	}
}
