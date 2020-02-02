XML SPaC [![Build Status](https://travis-ci.org/dylemma/xml-spac.svg?branch=master)](https://travis-ci.org/dylemma/xml-spac)
========


**API Docs:** [spac-core](https://javadoc.io/doc/io.dylemma/spac-core_2.13/0.8/io/dylemma/spac/index.html) | [xml-spac](https://javadoc.io/doc/io.dylemma/xml-spac_2.13/0.8/io/dylemma/spac/xml/index.html) | [json-spac](https://javadoc.io/doc/io.dylemma/json-spac_2.13/0.8/io/dylemma/spac/json/index.html)

**S**treaming **Pa**rser **C**ombinators is a Scala library for turning streams of "parser events" into strongly-typed data, for:

 - **XML** via [javax.xml.stream](https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/package-summary.html), a.k.a. "StAX"
 - **JSON** via [Jackson](https://github.com/FasterXML/jackson-core)

using handlers that are:

 - **Declarative** - You write *what* you want to get, not *how* to get it.
 - **Immutable** - Parsers that you create have no internal state.
 - **Composable** - Combine and transform parsers to handle complex data structures.
 - **Fast** - With minimal abstraction to get in the way, speed rivals any hand-written handler.
 - **Streaming** - Parse huge XML documents from events, not a DOM.

You can jump into a full [tutorial](tutorial.md), or check out the [examples](examples/src/main/scala/io/dylemma/xml/example),
but here's a taste of how you'd write a parser for a relatively-complex blog post XML structure:

```scala
val PostParser = (
	XMLParser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate) and
	XMLSplitter(* \ "author").first[Author] and
	XMLSplitter(* \ "stats").first[Stats] and
	XMLSplitter(* \ "body").first.asText and
	XMLSplitter(* \ "comments" \ "comment").asListOf[Comment]
).as(Post)
```

# Get it!

Add the following to your `build.sbt` file:

## For XML

```sbt
libraryDependencies += "io.dylemma" %% "xml-spac" % "0.8"
```

## For JSON

```sbt
libraryDependencies += "io.dylemma" %% "json-spac" % "0.8"
```

## To integrate another format yourself

```sbt
libraryDependencies += "io.dylemma" %% "spac-core" % "0.8"
```

# Main Concepts

SPaC is about handling streams of events, possibly transforming that stream, and eventually consuming it.

 - **`ConsumableLike[-Resource, +In]`** is the typeclass used to represent a "Stream",
   showing how a `Resource` can be treated as an Iterator/Traversable of `In` events.
   Implementations are provided for `String`, `InputStream`, `Reader`, and `File` resources.
 - **`Transformer[-In, +In2]`** is a stream processing step that converts a stream of `In` events
   to a stream of `In2` events.
    - `XMLTransformer[+In2]` is an alias for `Transformer[XMLEvent, In2]`
    - `JsonTransformer[+In2]` is an alias for `Transformer[JsonEvent, In2]`
 - **`Parser[-In, +Out]`** is a stream processing step that consumes a stream of `In` events to
   a single `Out` value.
    - `XMLParser[+Out]` is an alias for `Parser[XMLEvent, Out]`
    - `JsonParser[+Out]` is an alias for `Parser[JsonEvent, Out]`
 - **`Splitter[In, +Context]`** is a building block for `Transformer`s and `Parser`s.
   It "splits" a stream of `In` events into a *stream of streams* of `In` events,
   where each "substream" is associated with a `Context` value.
   The idea here is that if you know how to parse a certain sequence of events, you can easily
   extend that knowledge to parse a repetition of that sequence of events.
   You can also think of Splitter as a stream-based analog to an XPath.
    - `XMLSplitter` is available for xml-specific splitter semantics
    - `JsonSplitter` is available for json-specific splitter semantics

Instances of `Transformer`, `Parser`, and `Splitter` are *immutable*, meaning they can safely be
reused and shared at any time, even between multiple threads.
It's common to define an `implicit val fooParser: XMLParser[Foo] = /* ... */`

## Example

`XMLParser.forMandatoryAttribute("foo")` is a parser which will find the "foo" attribute of the first element it sees.

```xml
<!-- file: elem.xml -->
<elem foo="bar" />
```

```scala
val xml = new File("elem.xml")
val parser: XMLParser[String] = XMLParser.forMandatoryAttribute("foo")
val result: String = parser.parse(xml)
assert(result == "bar")
```

Suppose you have some XML with a bunch of `<elem foo="..."/>` and you want the "foo" attribute from each of them.
This is a job for a Splitter. You write an `XMLSplitter` sort of like an XPATH, to describe how to get to each element that you want to parse.

With the XML below, we want to parse the `<root>` element, since it represents the entire file.
We'll write our splitter by using the `*` matcher (representing the current element),
then selecting `<elem>` elements that are its direct children, using `* \ elem`.

```xml
<!-- file: root.xml -->
<root>
  <elem foo="bar" />
  <elem foo="baz" />
</root>
```

```scala
val xml = new File("root.xml")
val splitter: XMLSplitter[Unit] = XMLSplitter(* \ "elem")
val transformer: XMLTransformer[String] = splitter map parser

val rootParser: XMLParser[List[String]] = transformer.parseToList
val root: List[String] = rootParser.parse(xml)
assert(root == List("bar", "baz"))
```

Check out the docs for [ContextMatcherSyntax](https://javadoc.io/doc/io.dylemma/xml-spac_2.13/0.8/io/dylemma/spac/xml/syntax/ContextMatcherSyntax.html),
which defines helpers for creating the arguments to a `Splitter`, like the `*` value used above.

# Under the Hood

The underlying abstraction for processing "streams" is `Handler`.
`Handler` is allowed to be mutable, so that implementations can use utilities like `Builder`.
`Parser` and `Transformer` remain immutable by acting as *factories* for `Handler`.

```scala
trait Handler[-In, +Out] {
	def isFinished: Boolean
	def handleInput(input: In): Option[Out]
	def handleError(err: Throwable): Option[Out]
	def handleEnd(): Out
}
```

While processing a "stream", the `handleInput` method will be called for each `In` event.
The handler can indicate an early completion by returning `Some(out)`,
or indicate it is ready for more input by returning `None`.

At the end of the stream, `handleEnd` is used to force the handler to return an output.

When you call `parser.parse(source)`, the `source` is opened by an implicit `ConsumableLike`,
which then feeds events from the opened source into a fresh `Handler` until the handler
indicates an early return, or the stream reaches its end, at which point the `source` is closed.


```scala
trait ConsumableLike[-S, +In]{
    def getIterator(resource: S): Iterator[In] with AutoCloseable
	def apply[Out](source: S, handler: Handler[In, Out]): Out
}
```

The `apply` method asks the `source` (stream) to drive the `handler` until it produces a result `Out`.

There are many different `ConsumableLike` instances already, including generalized ones for `Iterable` collections and
`Iterator`s, and XML-specific ones for `String`, `File`, and `InputStream`. If you have a more specific "Stream" type,
you can [write your own `ConsumableLike[StreamType, EventType]`](core/src/main/scala/io/dylemma/spac/ConsumableLike.scala).

Here's how the core classes act like handler factories:

```scala
trait Parser[-In, +Out] extends (Any => Parser[In, Out]) {
	def makeHandler(): Handler[In, Out]
}

trait Transformer[-In, +In2] extends (Any => Transformer[In, In2] {
	def makeHandler[Out](downstream: Handler[In2, Out]): Handler[In, Out]
}
```
