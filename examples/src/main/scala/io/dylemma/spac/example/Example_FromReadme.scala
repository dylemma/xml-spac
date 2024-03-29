package io.dylemma.spac
package example

import cats.syntax.apply._
import io.dylemma.spac.xml._

import java.time.LocalDate
import java.time.format.DateTimeFormatter

object Example_FromReadme extends App {

	case class Post(date: LocalDate, author: Author, stats: Stats, body: String, comments: List[Comment])
	case class Author(id: String, name: String)
	case class Stats(numLikes: Int, numTweets: Int)
	case class Comment(date: LocalDate, author: Author, body: String)

	val rawXml = """<blog>
		|  <post date="2015-11-16">
		|    <author name="dylemma" id="abc123"/>
		|    <stats likes="123" tweets="4"/>
		|    <body>Hello world!</body>
		|    <comments>
		|      <comment date="2015-11-18">
		|        <author name="anonymous" id="def456"/>
		|        <body>I'm commenting on your fake blog!</body>
		|      </comment>
		|    </comments>
		|  </post>
		|  <post date="2015-11-18">
		|    <author name="johndoe" id="004200"/>
		|    <stats likes="7" tweets="1"/>
		|    <body>A second blog post, huzzah!</body>
		|    <comments>
		|      <comment date="2015-11-19">
		|        <author name="anonymous" id="def456"/>
		|        <body>It's me again</body>
		|      </comment>
		|    </comments>
		|  </post>
		|</blog>"""

	val commentDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
	val dateAttributeParser = XmlParser.attr("date").map(LocalDate.parse(_, commentDateFormat))

	implicit val AuthorParser: XmlParser[Author] = (
		XmlParser.attr("id"),
		XmlParser.attr("name")
	).mapN(Author.apply)

	val authorElementParser = Splitter.xml(* \ "author").as[Author].parseFirst

	implicit val StatsParser: XmlParser[Stats] = (
		XmlParser.attr("likes").map(_.toInt),
		XmlParser.attr("tweets").map(_.toInt)
	).mapN(Stats.apply)

	implicit val CommentParser: XmlParser[Comment] = (
		dateAttributeParser,
		authorElementParser,
		Splitter.xml(* \ "body").text.parseFirst
	).mapN(Comment.apply)

	implicit val PostParser: XmlParser[Post] = (
		dateAttributeParser,
		authorElementParser,
		Splitter.xml(* \ "stats").as[Stats].parseFirst,
		Splitter.xml(* \ "body").text.parseFirst,
		Splitter.xml(* \ "comments" \ "comment").as[Comment].parseToList
	).mapN(Post.apply)

	val postTransformer: Transformer[XmlEvent, Post] = Splitter.xml("blog" \ "post") joinBy PostParser
	val postTransformerAlt = Splitter.xml("blog" \ "post").as[Post] // available because PostParser is marked implicit

	postTransformer.parseTap(println) parse JavaxSource.fromString(rawXml)
}
