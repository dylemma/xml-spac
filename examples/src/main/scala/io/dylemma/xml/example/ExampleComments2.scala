package io.dylemma.xml.example

import java.text.SimpleDateFormat
import java.util.Date

import io.dylemma.xml.Parser
import io.dylemma.xml.ParsingDSL._
import play.api.libs.iteratee.Execution.Implicits.trampoline

/**
 * Created by dylan on 10/11/2015.
 */
object ExampleComments2 {

	case class Comment(date: Date, user: User, stats: Stats, body: String)
	case class User(id: String, name: String)
	case class Stats(upvoteCount: Int, downvoteCount: Int)

	// Parser for User
	implicit val UserParser: Parser[User] = (
		(* % "name") &
		(* % "id")
	).join(User)

	// Parser for Stats
	implicit val StatsParser: Parser[Stats] = (
		(* % "upvote-count").map(_.toInt) &
		(* % "downvote-count").map(_.toInt)
	).join(Stats)

	// note that SimpleDateFormat isn't thread-safe. You should use Joda time instead
	val commentDateFormat = new SimpleDateFormat("yyyy-MM-dd")
	// Parser for Comment
	implicit val CommentParser: Parser[Comment] = (
		(* % "date").map(commentDateFormat.parse) &
		(* / "user").as[User] &
		(* / "stats").as[Stats] &
		(* / "body" % Text)
	).join(Comment)

	def main(args: Array[String]) {

		val mainParser = (Root / "comments" / "comment").foreachResult[Comment](println)

		mainParser parse getClass.getResourceAsStream("/example-comments2.xml")
	}

}
