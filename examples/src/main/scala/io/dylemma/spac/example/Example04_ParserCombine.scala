package io.dylemma.spac
package example

import cats.syntax.apply._
import io.dylemma.spac.xml._

import java.time.LocalDate
import java.time.format.DateTimeFormatter

/**
 * Created by dylan on 10/11/2015.
 */
object Example04_ParserCombine {

	case class Comment(date: LocalDate, user: User, stats: Stats, body: String)
	case class User(id: String, name: String)
	case class Stats(upvoteCount: Int, downvoteCount: Int)

	val rawXml = JavaxSource.fromString("""<comments>
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
		|		<body>I made a new library to make XML parsing less awful! Try xml-spac, it'll change your life.</body>
		|	</comment>
		|</comments>""".stripMargin)

	/*
	Note that we're marking the `UserParser` and `StatsParser` below as implicit.
	This is for convenience so that we can use `splitter.as[User]` and `splitter.as[Stats]` later.
	 */

	// Parse <user id="..." name="..." /> into a User(id, name)
	implicit val UserParser: XmlParser[User] = (
		XmlParser.forMandatoryAttribute("id"),
		XmlParser.forMandatoryAttribute("name")
	).mapN(User.apply)

	// Parse <stats upvote-count="..." downvote-count="..." /> into a Stats(upvoteCount, downvoteCount)
	implicit val StatsParser: XmlParser[Stats] = (
		XmlParser.forMandatoryAttribute("upvote-count").map(_.toInt),
		XmlParser.forMandatoryAttribute("downvote-count").map(_.toInt)
	).mapN(Stats.apply)

	val commentDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
	// Parser for Comment
	implicit val CommentParser: XmlParser[Comment] = (
		XmlParser.forMandatoryAttribute("date").map(LocalDate.parse(_, commentDateFormat)),
		Splitter.xml(* \ "user").as[User].parseFirst,
		Splitter.xml(* \ "stats").as[Stats].parseFirst,
		Splitter.xml(* \ "body").text.parseFirst
	).mapN(Comment.apply)

	def main(args: Array[String]): Unit = {

		val mainParser = Splitter.xml("comments" \ "comment").as[Comment].parseTap(println)

		mainParser parse rawXml
	}

}
