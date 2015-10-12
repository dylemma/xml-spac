package io.dylemma.xml.example

import io.dylemma.xml.XMLEventEnumerator
import io.dylemma.xml.iteratee.ParsingDSL._
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
	implicit val UserParser = (
		(Elem % "name") ~
		(Elem % "id")
	)(User.apply _)

	// Parser for Stats
	implicit val StatsParser = (
		(Elem % "upvote-count") ~
		(Elem % "downvote-count")
	)(Stats.apply _)

	// Parser for Comment
	implicit val CommentParser = (
		(Elem % "date") ~
		(Elem \ "user").as[User] ~
		(Elem \ "stats").as[Stats] ~
		(Elem \ "body" \ Text)
	)(Comment.apply _)


	def main(args: Array[String]) {

		val source = XMLEventEnumerator(() => getClass.getResourceAsStream("/example-comments2.xml"))

		val mainParser = (Root \ "comments" \ "comment").consumeAs[Comment](//printlnConsumer)
			Iteratee.foreach(println)
		)

		source |>>> mainParser.toIteratee
	}

}
