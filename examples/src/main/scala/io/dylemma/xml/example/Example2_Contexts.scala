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

	Chained matchers have chain-like results. A "chain" is like a right-associative
	`shapeless.HList`, where the "endpoint" is at the start instead of the end.
	The most convenient way to interact with these chains is with the `~`
	extractor object that comes from `import syntax._` (analagous to `::` with HList)
	 */
	val chainedMatcher: ChainingContextMatcher[(String, String, Int), Start ~ String ~ String ~ Int] = {
		attr("title") \ (attr("author") & attr("id").map(_.toInt)) \ "comment"
	}

	/*
	Note that you generally don't need to think about what the chain type is.
	It's really just there to help the compiler figure out what the resulting
	matcher type should be when you use `\` to chain it to the next matcher.
	A ChainingContextMatcher is still just a ContextMatcher.
	 */
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
	The comment parser can't create a `Comment` without a `PostContext`,
	since that comes from an xml element further up the tree. We'll get
	that context from the `Splitter` (via the `through` method), so for
	now we only create a `PostContext => Parser[Comment]` to represent
	that dependency.
	 */
	case class Comment(body: String, context: PostContext)
	def commentParser(context: PostContext): Parser[Comment] = (
		Parser.forText and
		Parser.constant(context)
	).as(Comment)

	/*
	Note: for this particular use case it might be more convenient to
	insert the `context` with a `map` on the text parser.
	 */
	def alternateCommentParser(context: PostContext): Parser[Comment] = {
		Parser.forText.map(Comment(_, context))
	}

	/*
	With splitter's `through` method, the `commentParser` method will be
	called for each substream, with that substream's context as the argument.

	- `postSplitter through commentParser` creates a transformer
	- `[...] consumeForeach println` creates a consumer
	- `[...] consume rawXml` runs that consumer on the raw xml
	 */
	postSplitter through commentParser consumeForEach println consume rawXml
}
