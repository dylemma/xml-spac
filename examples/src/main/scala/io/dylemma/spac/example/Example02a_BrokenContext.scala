package io.dylemma.spac
package example

import cats.syntax.apply._
import io.dylemma.spac.xml._

object Example02a_BrokenContext extends App {

	// note the <post id="D"> - we are calling `_.toInt` on this context value,
	// which will throw an exception, which will end up wrapped in Error results
	// by the parser
	val rawXml = JavaxSource.fromString(s"""<blog>
		| <post id="D">
		|  <comment user="bob">Hello there</comment>
		|  <comment user="alice">Oh, hi</comment>
		| </post>
		| <post id="2">
		|  <comment user="carla">Test comment!</comment>
		|  <comment user="dylan">I'm testing too!</comment>
		| </post>
		|</blog>""".stripMargin)

	case class Comment(postId: Int, user: String, text: String)

	def CommentParser(idFromContext: Int): XmlParser[Comment] = (
		Parser.pure(idFromContext),
		XmlParser.forMandatoryAttribute("user"),
		XmlParser.forText
	).mapN(Comment.apply)

	val contextMatcher = "blog" \ ("post" & attr("id").map(_.toInt)) \ "comment"

	val consumer = Splitter.xml(contextMatcher)
		.map(CommentParser(_).wrapSafe) // parse the substreams created by the Splitter, using the implicit CommentParser
		.parseTap(println) // println each of the results

	consumer parse rawXml
}
