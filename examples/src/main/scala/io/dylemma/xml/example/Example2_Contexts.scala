package io.dylemma.xml.example

import xsp._
import xsp.syntax._

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
	As mentioned in Example1, Splitters can sometimes extract a "context".
	The context comes from elements in the tag stack, and will be passed
	to parsers that get attached to the associated splitter.

	The best place to start is with a `xsp.SingleElementContextMatcher`.
	These can be created manually, or you can take advantage of the
	convenience methods that come from `import syntax._`
	 */
	val titleAttributeMatcher: SingleElementContextMatcher[String] = attr("title")
	val idAttributeMatcher: SingleElementContextMatcher[String] = attr("id")

	// there's an implicit conversion for Strings to matchers, or you could call `elem(String)`
	val commentElementMatcher: SingleElementContextMatcher[Unit] = "comment"

	/*
	SingleElementContextMatchers can be combined with each other using `&` and `|`
	to create combinations of rules that apply to the element in their respective
	position.

	They can also be chained together using the `/` method.
	Doing so results in a `ChainingContextMatcher`, which still has `/`, but does
	not have the individual combiners like `&` and `|`.

	Chained matchers have chain-like results. A "chain" is actually just a nested
	Tuple2, where the right part is a value, but the right part could be another
	Tuple2. The most convenient way to interact with these chains is with the `~`
	extractor object that comes from `import syntax._`
	 */
	val chainedMatcher: ChainingContextMatcher[String ~ String ~ Int] = {
		attr("title") / (attr("author") & attr("id").map(_.toInt)) / "comment"
	}

	// the `String ~ String ~ Int` syntax is just a handy type alias
	val chainedMatcher2: ChainingContextMatcher[((String, String), Int)] = chainedMatcher

	/*
	You can map the results of a `ContextMatcher` using its `map`, `flatMap`,
	and `mapResult` methods. The `~` syntax will come in handy if several
	matchers have been chained.
	 */
	case class PostContext(blogTitle: String, author: String, postId: Int)
	val postContextMatcher = chainedMatcher map {
		// note that this is a type-safe match
		case title ~ author ~ postId => PostContext(title, author, postId)
	}

	/*
	Now you can use the `postContextMatcher` to create a splitter that
	has a `PostContext` as its context.
	 */
	val postSplitter = Splitter(postContextMatcher)

	/*
	Create a `Comment` class then create a parser for it.
	By using `Parser.forContext[PostContext]` we cause the resulting parser
	to *require* a `PostContext` as context. We combine it with the text from
	the parser's substream to create a Comment.
	 */
	case class Comment(body: String, context: PostContext)
	val postParser = Parser.compound(
		Parser.forText ~
		Parser.forContext[PostContext]
	).map(Comment.tupled)

	// assemble the consumer and run it on the xml
	postSplitter through postParser consumeForEach println consume rawXml

}
