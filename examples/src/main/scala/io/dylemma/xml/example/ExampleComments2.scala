package io.dylemma.xml.example

import io.dylemma.xml.{ ParsingDSL, XMLEventEnumerator }
import ParsingDSL._
import play.api.libs.iteratee.Execution.Implicits.trampoline
import play.api.libs.iteratee.Iteratee

/**
 * Created by dylan on 10/11/2015.
 */
object ExampleComments2 {

	case class Comment(date: String, user: User, stats: Stats, body: String)
	case class User(id: String, name: String)
	case class Stats(upvoteCount: String, downvoteCount: String)

	// Parser for User
	implicit val UserParser: AnyContextParser[User] = (
		(* % "name") &
		(* % "id")
	).join(User)

	// Parser for Stats
	implicit val StatsParser: AnyContextParser[Stats] = (
		(* % "upvote-count") &
		(* % "downvote-count")
	).join(Stats)

	// Parser for Comment
	implicit val CommentParser: AnyContextParser[Comment] = (
		(* % "date") &
		(* / "user").as[User] &
		(* / "stats").as[Stats] &
		(* / "body" % Text)
	).join(Comment)

	def main(args: Array[String]) {

		val source = XMLEventEnumerator(() => getClass.getResourceAsStream("/example-comments2.xml"))

		val mainParser = (Root / "comments" / "comment").foreach[Comment](println)

		source |>>> mainParser.toIteratee()
	}

}
