package io.dylemma.spac
package example

import cats.syntax.apply._
import io.dylemma.spac.xml._
import io.dylemma.spac.xml.spac_javax._
import org.joda.time.LocalDate
import org.joda.time.format.DateTimeFormat

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

	val commentDateFormat = DateTimeFormat.forPattern("yyyy-MM-dd")
	val dateAttributeParser = XmlParser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate)

	implicit val AuthorParser: XmlParser[Author] = (
		XmlParser.forMandatoryAttribute("id"),
		XmlParser.forMandatoryAttribute("name")
	).mapN(Author)

	val authorElementParser = Splitter.xml(* \ "author").as[Author].into.first

	implicit val StatsParser: XmlParser[Stats] = (
		XmlParser.forMandatoryAttribute("likes").map(_.toInt),
		XmlParser.forMandatoryAttribute("tweets").map(_.toInt)
	).mapN(Stats)

	implicit val CommentParser: XmlParser[Comment] = (
		dateAttributeParser,
		authorElementParser,
		Splitter.xml(* \ "body").text.into.first
	).mapN(Comment)

	implicit val PostParser: XmlParser[Post] = (
		dateAttributeParser,
		authorElementParser,
		Splitter.xml(* \ "stats").as[Stats].into.first,
		Splitter.xml(* \ "body").text.into.first,
		Splitter.xml(* \ "comments" \ "comment").as[Comment].into.list
	).mapN(Post)

	val postTransformer: Transformer[XmlEvent, Post] = Splitter.xml("blog" \ "post") joinBy PostParser
	val postTransformerAlt = Splitter.xml("blog" \ "post").as[Post] // available because PostParser is marked implicit

	postTransformer.into.tap(println) parse rawXml
}