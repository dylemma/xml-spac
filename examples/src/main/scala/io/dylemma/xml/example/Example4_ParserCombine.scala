package io.dylemma.xml.example

import java.text.SimpleDateFormat
import java.util.Date

import io.dylemma.spac._
import io.dylemma.spac.xml._

/**
 * Created by dylan on 10/11/2015.
 */
object Example4_ParserCombine {

	case class Comment(date: Date, user: User, stats: Stats, body: String)
	case class User(id: String, name: String)
	case class Stats(upvoteCount: Int, downvoteCount: Int)

	val rawXml = """<comments>
		|	<comment date="2014-07-20">
		|		<user name="alice" id="98qja34j3"/>
		|		<stats upvote-count="123" downvote-count="20"/>
		|		<body>Parsing XML sure is a pain...</body>
		|	</comment>
		|	<comment date="2014-07-21">
		|		<user name="bob" id="3adfngfl"/>
		|		<stats upvote-count="10" downvote-count="2"/>
		|		<body>Yeah, I use SAX, but it's still pretty tedious</body>
		|	</comment>
		|	<comment date="2014-07-21">
		|		<user name="alice" id="98qja34j3"/>
		|		<stats upvote-count="15" downvote-count="3"/>
		|		<body>I tried StAX. It seems nicer, but still fairly repetitive</body>
		|	</comment>
		|	<comment date="2015-10-11">
		|		<user name="dylemma" id="038oqfdje"/>
		|		<stats upvote-count="23841" downvote-count="4"/>
		|		<body>I made a new library to make XML parsing less awful! Try xml-stream, it'll change your life.</body>
		|	</comment>
		|</comments>""".stripMargin

	/*
	Note that we're marking the `UserParser` and `StatsParser` below as implicit.
	This is simply for convenience so that we can use `Splitter(...).first[User]`
	and `Splitter(...).first[Stats]` later on.
	 */

	// Parse <user id="..." name="..." /> into a User(id, name)
	implicit val UserParser: Parser[User] = (
		Parser.forMandatoryAttribute("id") and
		Parser.forMandatoryAttribute("name")
	).as(User)

	// Parse <stats upvote-count="..." downvote-count="..." /> into a Stats(upvoteCount, downvoteCount)
	implicit val StatsParser: Parser[Stats] = (
		Parser.forMandatoryAttribute("upvote-count").map(_.toInt) and
		Parser.forMandatoryAttribute("downvote-count").map(_.toInt)
	).as(Stats)

	// note that SimpleDateFormat isn't thread-safe. You should use Joda time instead
	val commentDateFormat = new SimpleDateFormat("yyyy-MM-dd")
	// Parser for Comment
	implicit val CommentParser: Parser[Comment] = (
		Parser.forMandatoryAttribute("date").map(commentDateFormat.parse) and
		Splitter(* \ "user").first[User] and
		Splitter(* \ "stats").first[Stats] and
		Splitter(* \ "body").first.asText
	).as(Comment)

	def main(args: Array[String]) {

		val mainParser = Splitter("comments" \ "comment").as[Comment].consumeForEach(println)

		mainParser consume rawXml
	}

}
