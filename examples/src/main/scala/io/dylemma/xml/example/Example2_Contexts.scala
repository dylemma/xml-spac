package io.dylemma.xml.example

import javax.xml.stream.events.XMLEvent
import scala.concurrent.Future

import io.dylemma.xml.ParsingDSL._
import io.dylemma.xml._
import play.api.libs.iteratee.{ Enumerator, Iteratee }

/*
 * The `trampoline` execution context runs everything on the current thread,
 * so all of our parser/future/iteratee operations in this file will block.
 */
import play.api.libs.iteratee.Execution.Implicits.trampoline

object Example2_Contexts extends App {

	val rawXml = s"""<blog title="Cool Beans">
		| <post id="1">
		|  <comment user="bob">Hello there</comment>
		|  <comment user="alice">Oh, hi</comment>
		| </post>
		| <post id="2">
		|  <comment user="carla">Test comment!</comment>
		|  <comment user="dylan">I'm testing too!</comment>
		| </post>
		|</blog>""".stripMargin

	/** This creates an Enumerator (stream) of XMLEvents from the `rawXml` String.
		* The `XMLEventEnumerator` constructor uses the `AsInputStream` typeclass to
		* accept arguments of various types, including Strings, Files, and InputStreams.
		*/
	val xmlStream: Enumerator[XMLEvent] = XMLEventEnumerator(rawXml)

	/** This class represents the 'context' that our parser *must* be in,
		* in order to generate results. We'll be taking the `blogTitle` from
		* the "blog" element's "title" attribute, and the `postId` from the
		* "post" element's "id" attribute.
		*/
	case class PostContext(blogTitle: String, postId: Int)

	/** A splitter divides the XMLEvent stream into substreams, where each
		* substream has some defined context. You attach a parser to a splitter
		* in order to transform a stream of XMLEvents to a stream of results.
		*
		* There's a lot of syntax sugar happening in this expression:
		*
		* The strings like "blog", "post", and "comment" are being implicitly
		* transformed to `Matcher`s which filter on elements with those tags.
		*
		* The `attr("title")` and `attr("id")` calls return `Matcher`s which
		* extract their respective attributes from the element they are attached to.
		*
		* Individual matchers are combined with `&`. They are then combined with `/`
		* to create an overall matcher that applies to the entire tag stack.
		*
		* When multiple matchers which extract results are combined, their results
		* are combined into a `Chain`. The `joinContext` call at the end combines
		* each result from the extracted chain by passing them into a function -
		* in this case - `PostContext.apply`.
		*/
	val splitter: Splitter[PostContext] = (
		Root /
		("blog" & attr("title")) /
		("post" & attr("id").map(_.toInt)) /
		"comment"
	).joinContext(PostContext)

	/** This class is what we are parsing.
		* The `context` argument will come from the `blog[title]` and `post[id]`
		* attributes, which are being extracted by the `splitter`.
		* The `user` argument will come from the `comment[user]` attribute.
		* The `text` argument will come from the text inside the `comment` element.
		*/
	case class Comment(context: PostContext, user: String, text: String)

	/** We define the parser for `Comments`.
		* We're actually defining 3 separate parsers, combining them into a chain via `&`,
		* then `join`-ing the chain together with the `Comment.apply` function.
		*/
	val commentParser: ParserForContext[PostContext, Comment] = (
		/* This parser simply returns the context that gets passed in,
		 * but it also adds the requirement that there *be* a conext.
		 */
		inContext[PostContext] &

		/* This parser extracts the "user" attribute from the top-level element
		 * from the xml stream. Since this will be called on substreams where
		 * "comment" is the top-level element, it'll find the `comment[user]` attribute.
		 */
		(* % "user") &

		/* This parser extracts the text from the top-level element of the xml stream.
		 * Again, this parser will be called on substreams where "comment" is the
		 * top-level element, so it'll find the comment's content.
		 */
		(* % Text)

	/* Before calling `join`, we have a `Parser[PostContext, Chain[Chain[PostContext, String], String]]]`.
	 * The `join` function will take a `(PostContent, String, String) => Result`.
	 * Here, we use `Comment.apply`
	 */
	).join(Comment)

	/* The `commentParser` can't be used by itself because it requires a context
	 * to be passed into it. You can use the `inContext` method to give it context,
	 * or you can attach it to the `splitter`, which will pass the appropriate
	 * context for each substream it finds.
	 *
	 * We'll attach it to the `splitter`, which gives us a `Transformer`.
	 */
	val commentTransformer = splitter.through(commentParser)

	/*
	 * Under the hood, a transformer is an Enumeratee[XMLEvent, Result], meaning
	 * it transforms a stream of xml events into a stream of results.
	 * You can use the usual `Enumerator &> Enumeratee |>>> Iteratee` combinations
	 * to consume the stream.
	 */
	val commentEnumerator: Enumerator[Result[Comment]] = xmlStream &> commentTransformer.toEnumeratee
	val commentIteratee = Iteratee.foreach[Result[Comment]]{ result => println(s"Iteratee result: $result") }
	val doneParsing: Future[Unit] = commentEnumerator |>>> commentIteratee

	/* ...BUT you don't *have* to.
	 * The transformer has some convenience methods for collecting the elements it
	 * emits into some further value (e.g. a collection or an option), resulting in
	 * a parser for that value. Since the transformer already had a context attached,
	 * the resulting parser will also already have a context attached.
	 */
	val firstCommentParser: Parser[Comment] = commentTransformer.parseSingle
	val commentsListParser: Parser[List[Comment]] = commentTransformer.parseList
	val printlnCommentParser: Parser[Unit] = commentTransformer.foreach(println)

	/* Under the hood, a Parser is an Iteratee[XMLEvent, Result], meaning
	 * it consumes a stream of XMLEvents to generate some result value. You can
	 * consume an Enumerator via the `|>>>` or `run` methods.
	 */
	val allComments: Future[Result[List[Comment]]] = xmlStream |>>> commentsListParser.toIteratee

	/* For convenience, you could just call `parse` instead, avoiding having to
	 * deal with all of the low-level Enumerator/Enumeratee/Iteratee details.
	 */
	val allComments2: Future[Result[List[Comment]]] = commentsListParser.parse(rawXml)
	allComments2 onComplete { tryResult => println(s"allComments2 finished: $tryResult") }
}
