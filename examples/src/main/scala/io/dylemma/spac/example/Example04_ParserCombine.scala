package io.dylemma.spac
package example

import java.text.SimpleDateFormat
import java.util.Date

import cats.syntax.apply._
import io.dylemma.spac.xml._
import io.dylemma.spac.xml.JavaxSupport._

/**
 * Created by dylan on 10/11/2015.
 */
object Example04_ParserCombine {

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
	This is simply for convenience so that we can use `XMLSplitter(...).first[User]`
	and `XMLSplitter(...).first[Stats]` later on.
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

	// note that SimpleDateFormat isn't thread-safe. You should use Joda time instead
	val commentDateFormat = new SimpleDateFormat("yyyy-MM-dd")
	// Parser for Comment
	implicit val CommentParser: XmlParser[Comment] = (
		XmlParser.forMandatoryAttribute("date").map(commentDateFormat.parse),
		Splitter.xml(* \ "user").as[User].parseFirst,
		Splitter.xml(* \ "stats").as[Stats].parseFirst,
		Splitter.xml(* \ "body").text.parseFirst
	).mapN(Comment.apply)

	def main(args: Array[String]): Unit = {

		val mainParser = Splitter.xml("comments" \ "comment").as[Comment].parseTap(println)

		mainParser parse rawXml
	}

}
