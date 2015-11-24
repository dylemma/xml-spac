package io.dylemma.xml.example

import io.dylemma.xml.ParserForContext
import io.dylemma.xml.ParsingDSL._
import play.api.libs.iteratee.Execution.Implicits.trampoline

/**
 * Created by dylan on 11/6/2015.
 */
object ExampleWithContext extends App {

	val rawXml = s"""<blog>
		| <post id="1">
		|  <comment user="bob">Hello there</comment>
		|  <comment user="alice">Oh, hi</comment>
		| </post>
		| <post id="2">
		|  <comment user="carla">Test comment!</comment>
		|  <comment user="dylan">I'm testing too!</comment>
		| </post>
		|</blog>""".stripMargin

	case class Comment(postId: Int, user: String, text: String)

	implicit val CommentParser: ParserForContext[Int, Comment] = (
		inContext[Int] &
		(* % "user") &
		(* % Text)
	).join(Comment)

	val contextMatcher = Root / "blog" / ("post" & attr("id")) / "comment" mapContext (_.toInt)

	val handler = contextMatcher.foreach[Comment](println)

	handler parse rawXml
}
