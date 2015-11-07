package io.dylemma.xml.example

import io.dylemma.xml.{ ParsingDSL, XMLEventEnumerator }
import ParsingDSL._
import play.api.libs.iteratee.Execution.Implicits.trampoline

/**
 * Created by dylan on 10/11/2015.
 */
object ExampleComments extends App {

	// simple class to represent a comment
	case class Comment(user: String, body: String)

	/** Create a Parser[Comment] using the functional builder syntax.
		*
		* This syntax allows you to join multiple parsers together using `~`,
		* then combine their results with a function.
		*
		* Note the use of `Elem` instead of `Root`. If you used `Root`, you'd
		* need to specify it as `Root \ "comment" ...`.
		*/
	implicit val CommentParser = (
		(* % "user") &
		(* % Text)
	).join(Comment)

	val source = XMLEventEnumerator(() => getClass.getResourceAsStream("/example-comments.xml"))

	val printlnParser = (Root / "comments" / "comment").foreach[Comment](println)

	source |>>> printlnParser.toIteratee()
}
