package io.dylemma.spac
package example

import cats.syntax.apply._
import io.dylemma.spac.xml.XmlEvent.ElemStart
import io.dylemma.spac.xml._
import io.dylemma.spac.xml.spac_javax._

object Example2_Contexts extends App {

	val rawXml = s"""<blog title="Cool Beans">
		| <post id="1" author="Dylan">
		|  <comment>Hello there</comment>
		|  <comment>Oh, hi</comment>
		| </post>
		| <post id="2" author="Bob">
		|  <comment>Test comment!</comment>
		|  <comment>I'm testing too!</comment>
		| </post>
		|</blog>""".stripMargin

	/*
	This example introduces `ContextMatcher`s, which are responsible for
	extracting "context" values based on an XML tag stack, and providing
	them to an associated `Splitter`. (see Example1 for info on Splitters).

	A `ContextMatcher[A]` extracts a context value of type `A` from the XML
	tag stack, and may look at any number of elements to do so. Context
	matchers have a variety of transformation and combination methods like
	`map`, `flatMap`, `filter`, `\\` (backslash), and `or`.
	A more specific `ContextMatcher` is the `SingleElementContextMatcher[A]`
	which extracts a context from the first element (bottom) of the stack,
	and has the additional combination method, `and`.
	When constructing a custom ContextMatcher, you will typically start with
	a single-element matcher, then use the `\` operator to chain to another.

	By importing `spac.syntax._`, you get access to several convenience methods
	for creating matchers. Most notable are `elem` and `attr`.
	*/

	// extracts the "title" attribute from the first element in the stack,
	// but does not match if that element is missing the "title" attribute.
	val titleAttributeMatcher: SingleItemContextMatcher[ElemStart, String] = attr("title")

	// same as above, but for the "id" attribute
	val idAttributeMatcher: SingleItemContextMatcher[ElemStart, String] = attr("id")

	// extracts nothing, and only matches if the name of the element is "comment"
	val commentElementMatcher: SingleItemContextMatcher[ElemStart, Unit] = elem("comment")

	// the `syntax._` import gives an implicit conversion from `String` or `QName` to a matcher
	val commentElementMatcher2: SingleItemContextMatcher[ElemStart, Unit] = "comment"

	/*
	The base `ContextMatcher` trait has two type parameters:
	 1. The "context stack type"
	 2. The context match output type
	Since we're dealing with XML, we can use the `XMLContextMatcher` type alias,
	which uses `StartElement` as the "context stack type"
	 */

	/*
	ContextMatchers can be chained together with the `\` (backslash) method.
	When combining two matchers as a chain, their two context types will be
	combined as a Tuple2, then reduced according to the rules of the
	`TypeReduce` typeclass. Essentially, `Unit` will be stripped out unless
	it is the only thing left.
	*/
	val chainExample1: XmlContextMatcher[Unit] = "blog" \ "post" \ "comment"
	val chainExample2: XmlContextMatcher[String] = "blog" \ attr("id") \ "comment"
	val chainExample3: XmlContextMatcher[(String, String)] = "blog" \ attr("id") \ attr("stuff")

	/*
	The same type reduction logic also applies when combining two single-element
	matchers with the `&` ("and") method. The matcher below will extract both the
	"id" and "author" attributes from the first element in the stack. If the context
	is matched successfully, the result will be an `(id, author)` tuple.
	*/
	val andExample1: XmlContextMatcher[(String, String)] = attr("id") & attr("author")

	/*
	Complicated context matchers can be built from the combination methods
	 */
	val chainedMatcher: XmlContextMatcher[(String, (String, Int))] = {
		// The individual segments are [String] + [(String, Int)] + [Unit].
		// Unit is stripped, leaving a tuple of `String` and `(String, Int)`
		attr("title") \ (attr("author") & attr("id").map(_.toInt)) \ "comment"
	}

	/*
	You can map the results of a `ContextMatcher` using its `map` method (shown below),
	as well as its `flatMap` and `filter` methods (not shown).
	 */
	case class PostContext(blogTitle: String, author: String, postId: Int)
	val postContextMatcher: XmlContextMatcher[PostContext] = chainedMatcher map {
		case (title, (author, postId)) => PostContext(title, author, postId)
	}

	/*
	Now you can use the `postContextMatcher` to create a splitter that
	has a `PostContext` as its context.
	 */
	val postSplitter: XmlSplitter[PostContext] = Splitter.xml(postContextMatcher)

	/*
	Create a `Comment` class then create a parser for it.
	The comment parser can't create a `Comment` without a `PostContext`,
	since that comes from an xml element further up the tree. We'll get
	that context from the `Splitter` (via the `map` method), so for
	now we only create a `PostContext => XMLParser[Comment]` to represent
	that dependency.
	 */
	case class Comment(body: String, context: PostContext)
	def commentParser(context: PostContext): XmlParser[Comment] = {
		(XmlParser.forText, Parser.pure(context)).mapN(Comment)
	}

	/*
	Note: for this particular use case it might be more convenient to
	insert the `context` with a `map` on the text parser.
	 */
	def alternateCommentParser(context: PostContext): XmlParser[Comment] = {
		XmlParser.forText.map(Comment(_, context))
	}

	/*
	With splitter's `map` method, the `commentParser` method will be
	called for each substream, with that substream's context as the argument.

	- `postSplitter map commentParser` creates a transformer
	- `[...] parseForeach println` creates a consumer
	- `[...] consume rawXml` runs that consumer on the raw xml
	 */
	postSplitter
		.map(commentParser)
		.into.tap(println)
		.parse(rawXml)
}
