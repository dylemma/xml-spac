package io.dylemma.xml.example

import io.dylemma.spac._

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

	The simplest `ContextMatcher` type is `ContextMatcher[A]`, which extracts
	contexts of type `A`.

	After that, there's `ChainingContextMatcher[A, AChain]`. Chaining matchers
	can be combined with each other to form a chain. This is the most commonly-
	used type of context matcher. A `ChainingContextMatcher` uses an internal
	representation of its results as a "Chain", which helps the compiler figure
	out how to combine them. Most times, you will not need to know the actual
	chain type, but for the sake of the example they will be explicitly stated.

	Finally, there's `SingleElementContextMatcher[A, AChain]`. These matchers
	are a special kind of chaining matcher which always operates on exactly
	one element from the stack. When creating custom context matchers, you will
	usually start with one of these, then chain it with another.

	By importing `spac.syntax._`, you get access to several convenience methods
	for creating matchers. Most notable are `elem` and `attr`.
	 */
	val titleAttributeMatcher: SingleElementContextMatcher[String, Start ~ String] = attr("title")
	val idAttributeMatcher: SingleElementContextMatcher[String, Start ~ String] = attr("id")
	val commentElementMatcher: SingleElementContextMatcher[Unit, Start] = elem("comment")

	// you can also implicitly call `elem` on a `String` or a `QName`
	val commentElementMatcher2: SingleElementContextMatcher[Unit, Start] = "comment"

	/*
	SingleElementContextMatchers can be combined with each other using `&` and `|`
	to create combinations of rules that apply to the element in their respective
	position.

	They can also be chained together using the `\` (backslash) method.
	Doing so results in a `ChainingContextMatcher`, which still has `\`, but does
	not have the individual combiners like `&` and `|`.

	Chained matchers have chain-like results. A "chain" is actually just a nested
	Tuple2, where the right part is a value, but the right part could be another
	Tuple2. The most convenient way to interact with these chains is with the `~`
	extractor object that comes from `import syntax._`
	 */
	val chainedMatcher: ChainingContextMatcher[(String, String, Int), Start ~ String ~ String ~ Int] = {
		attr("title") \ (attr("author") & attr("id").map(_.toInt)) \ "comment"
	}

	// remember a chaining context matcher is still just a context matcher
	val chainedMatcher2: ContextMatcher[(String, String, Int)] = chainedMatcher

	/*
	You can map the results of a `ContextMatcher` using its `map`, `flatMap`,
	and `mapWith` methods.
	 */
	case class PostContext(blogTitle: String, author: String, postId: Int)
	val postContextMatcher: ContextMatcher[PostContext] = chainedMatcher map PostContext.tupled

	/*
	Now you can use the `postContextMatcher` to create a splitter that
	has a `PostContext` as its context.
	 */
	val postSplitter: XmlSplitter[PostContext] = Splitter(postContextMatcher)

	/*
	Create a `Comment` class then create a parser for it.
	By using `Parser.forContext[PostContext]` we cause the resulting parser
	to *require* a `PostContext` as context. We combine it with the text from
	the parser's substream to create a Comment.
	 */
	case class Comment(body: String, context: PostContext)
	val postParser: Parser[PostContext, Comment] = (
		Parser.forText and
		Parser.forContext[PostContext]
	).as(Comment)

	// assemble the consumer and run it on the xml
	postSplitter through postParser consumeForEach println consume rawXml

}
