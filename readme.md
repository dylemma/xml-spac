XML SPaC
========

[![Build Status](https://travis-ci.org/dylemma/xml-spac.svg?branch=master)](https://travis-ci.org/dylemma/xml-spac)

**XML** **S**treaming **Pa**rser **C**ombinators is a Scala library for creating event-based
[StAX](https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/package-summary.html) parsers that are:

 - **Declarative** - You write *what* you want to get, not *how* to get it.
 - **Immutable** - Parsers that you create have no internal state.
 - **Composable** - Combine and transform parsers to handle complex data structures. 
 - **Fast** - With minimal abstraction to get in the way, speed rivals any hand-written handler.
 - **Streaming** - Parse huge XML documents from events, not a DOM.

You can jump into a full [tutorial](tutorial.md), or check out the [examples](examples/src/main/scala/io/dylemma/xml/example),
but here's a taste of how you'd write a parser for a relatively-complex blog post XML structure:

```scala
val PostParser = (
	Parser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate) and
	Splitter(* \ "author").first[Author] and
	Splitter(* \ "stats").first[Stats] and
	Splitter(* \ "body").first.asText and
	Splitter(* \ "comments" \ "comment").asListOf[Comment]
).as(Post)
```

# Get it!

Add the following to your `build.sbt` file:

```sbt
libraryDependencies += "io.dylemma" %% "xml-spac" % "0.2"
```

*Note: the readme and tutorial documents currently reflect the upcoming 0.3-SNAPSHOT API. 
For documentation relevant to the latest release (v0.2), please refer to [the old readme](https://github.com/dylemma/xml-spac/blob/0.2/readme.md)*

# Main Concepts

The classes you'll interact with most in **XML SPaC** are `Parser`, `Transformer`, `Splitter`, and `Consumer`.  

A **`Consumer[In, Out]`** knows how to consume a stream of `In` events, producing an `Out` result.

A **`Parser[Out]`** is like a `Consumer` of [XMLEvents](https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/events/XMLEvent.html)
which wraps its results in a `Try[Out]`.

A **`Transformer[A, B]`** turns a stream of `A` events into a stream of `B` events.

A **`Splitter[In, Context]`** combines with a `Context => Parser[Out]` to create a `Transformer[In, Out]`.  
A Splitter "splits" a stream into "substreams", assigning a context value to each substream.
A parser can be run on each of the substreams, and the resulting values become the contents of the transformed stream.

Note that each of the four classes mentioned here are completely immutable, 
which means they can be shared and combined without worrying about state or thread safety.
All of the *mutable* stuff is encapsulated in a `Handler` instance, which is created each time you want to process a stream.
For the most part, you won't need to interact with Handlers; the library will do it for you.

## Example

`Parser.forMandatoryAttribute("foo")` is a parser which will find the "foo" attribute of the first element it sees.

```scala
val parser = Parser forMandatoryAttribute "foo"
val result = parser parse """<elem foo="bar"/>"""
assert(result == Success("bar"))
```

Suppose you have some XML with a bunch of `<elem foo="..."/>` and you want the "foo" attribute from each of them.
This is a job for a Splitter. You write an XML Splitter sort of like an XPATH, to describe how to get to each element that you want to parse.

```scala
val xml = """<root><elem foo="bar"/><elem foo="baz"/></root>"""
val splitter = Splitter(* \ "elem")
val transformer = splitter through parser

val rootConsumer = transformer.consumeToList
assert(rootConsumer.consume(xml) == List("bar", "baz"))

val rootParser = transformer.parseToList
assert(rootParser.parse(xml) == Success(List("bar", "baz")))
```

Check out the docs for [ContextMatcherSyntax](http://static.javadoc.io/io.dylemma/xml-spac_2.11/0.2/index.html#io.dylemma.spac.syntax.ContextMatcherSyntax),
which defines helpers for creating the arguments to a `Splitter`, 
and [Parser](http://static.javadoc.io/io.dylemma/xml-spac_2.11/0.2/index.html#io.dylemma.spac.Parser$),
which provides lots of convenience implementations of xml parsers.

# Under the Hood

The core classes are able to be immutable because they act as factories for "handlers", 
which contain all of the mutable state and stream processing logic.
The `Handler` class is fairly simple: 

```scala
trait Handler[-In, +Out] {
	def isFinished: Boolean
	def handleInput(input: In): Option[Out]
	def handleError(err: Throwable): Option[Out]
	def handleEnd(): Out
}
```

When the stream wants to send a value, it calls `handleInput`. 
When the stream ends, it calls `handleEnd`.
When something goes wrong, it calls `handleError`.
If the `handleInput` or `handleError` methods returned a `Some`, the stream can stop and clean up.
If the handler `isFinished` at any point, the stream can stop and clean up.

Now, I said "stream" a lot in the previous paragraph, but there is no "Stream" class in **XML SPaC**.
Instead, there's the `ConsumableLike[S, In]` typeclass, which says that `S` is a stream of `In`.
For example, `List[A]` could be a stream of `A`, so there would be an instance of `ConsumableLike[List[A], A]`.
An `InputStream` could be a stream of `XMLEvent`, so there would be an instance of `ConsumableLike[InputStream, XMLEvent]`.


```scala
trait ConsumableLike[-S, +In]{
	def apply[Out](source: S, handler: Handler[In, Out]): Out
}
```

The `apply` method asks the `source` (stream) to drive the `handler` until it produces a result `Out`.

There are many different `ConsumableLike` instances already, including generalized ones for `Iterable` collections and
`Iterator`s, and XML-specific ones for `String`, `File`, and `InputStream`. If you have a more specific "Stream" type,
you can [write your own `ConsumableLike[StreamType, EventType]`](https://github.com/dylemma/xml-spac/blob/master/core/src/main/scala/io/dylemma/spac/ConsumableLike.scala).

Here's how the core classes act like handler factories:

```scala
trait Consumer[-In, +Out] {
	def makeHandler(): Handler[In, Out]
}

trait Parser[+Out] {
	def makeHandler(): Handler[XMLEvent, Try[Out]] 
}

trait Transformer[In, B] {
	def makeHandler[Out](downstream: Handler[B, Out]): Handler[In, Out]
}

```
