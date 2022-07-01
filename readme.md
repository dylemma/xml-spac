spac
====

Spac is a Scala library for building stream consumers in a declarative style, specialized for tree-like data types like XML and JSON. Parsers are:

 - **Declarative** - You write *what* you want to get, not *how* to get it
 - **Immutable** - Parsers may be shared and reused without worry
 - **Composable** - Combine and transform parsers to handle complex data structures
 - **Fast** - With minimal abstraction to get in the way, speed rivals any hand-written handler
 - **Streaming** - Parse huge XML/JSON documents from events, not a DOM

```scala
implicit val PostParser: XmlParser[Post] = (
  XmlParser.attr("date").map(LocalDate.parse(_, commentDateFormat)),
  Splitter.xml(* \ "author").as[Author].parseFirst,
  Splitter.xml(* \ "stats").as[Stats].parseFirst,
  Splitter.xml(* \ "body").text.parseFirst,
  Splitter.xml(* \ "comments" \ "comment").as[Comment].parseToList
).mapN(Post)

Splitter
   .xml("blog" \ "post")
   .as[Post]
   .parseTap { println }
   .parse(JavaxSource.fromFile { new File("./blog.xml") })
```

It delegates to a backend of your choice to obtain a stream of events:

 - **XML** support via:
   - [javax.xml.stream](https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/package-summary.html), a.k.a. "StAX"
   - [fs2-data-xml](https://fs2-data.gnieh.org/)
 - **JSON** support via:
   - [Jackson](https://github.com/FasterXML/jackson-core)
   - [fs2-data-json](https://fs2-data.gnieh.org/)

# Resources

- The [tutorial](https://github.com/dylemma/xml-spac/blob/main/tutorial.md)
- The [examples](https://github.com/dylemma/xml-spac/tree/main/examples/src/main/scala/io/dylemma/spac/example)
- The [API docs](http://dylemma.github.io/xml-spac/api/latest)
- This file (keep scrolling!)

# Setup

Add (your choice of) the following to your `build.sbt` file:

```sbt
val spacVersion = "0.10.0" 
libraryDependencies ++= Seq(
  "io.dylemma" %% "spac-core" % spacVersion,         // core classes like Parser and Transformer 
  "io.dylemma" %% "spac-interop-fs2" % spacVersion,  // adds interop with fs2.Stream and fs2.Pipe
   
  "io.dylemma" %% "xml-spac" % spacVersion,          // classes for XML-specific parsers
  "io.dylemma" %% "xml-spac-javax" % spacVersion,    // XML parser backend using javax.xml.stream
  "io.dylemma" %% "xml-spac-fs2-data" % spacVersion, // XML parser backend using fs2-data-xml

  "io.dylemma" %% "json-spac" % spacVersion,         // classes for JSON-specific parsers
  "io.dylemma" %% "json-spac-jackson" % spacVersion, // JSON parser backend using the Jackson library
  "io.dylemma" %% "json-spac-fs2-data" % spacVersion // JSON parser backend using fs2-data-json
)
```

# Main Concepts

SPaC is about handling streams of events, possibly transforming that stream, and eventually consuming it.

## Parser

A `Parser[In, Out]` consumes a series of `In` values to produce a single `Out` value.

Parsers don't necessarily have to consume the entire series to produce a result; 
for example `Parser.firstOpt[A]` can produce a result upon encountering the first value.

The `xml-spac` and `json-spac` modules define useful type aliases:

- `type XmlParser[Out] = Parser[XmlEvent, Out]`
- `type JsonParser[Out] = Parser[JsonEvent, Out]`

Parsers are [`Applicative`](https://typelevel.org/cats/typeclasses/applicative.html) with respect to the `Out` type,
which means you can combine them:

```scala
val parser1: Parser[In, A] = /* ... */
val parser2: Parser[In, B] = /* ... */

import cats.syntax.apply._
val combined: Parser[In, (A, B)] = (parser1, parser2).tupled
```

## Transformer

A `Transformer[In, Out]` transforms a series of `In` values to a series of `Out` values.

Transformers are typically created as an intermediate step on the way to creating a Parser.
You'll usually call one of a Transformer's `parseTo*` methods to convert it to a Parser.

## Splitter

A `Splitter[In, Context]` identifies "substreams" in a stream of `In` values, with each "substream" identified by a `Context`.

Think of `Splitter` like XPath; for example, if you have a series of `XmlEvent`s representing an XML document, 
and you want to parse only a certain type of element, you use a `Splitter` to locate those elements.
Each "substream" would represent the `XmlEvent`s for one such element.

You can attach a `Context => Parser[In, Out]` function to the Splitter so that each "substream" is passed through the
given Parser so they will each be treated as an `Out`, and every event outside of a "substream" will be ignored.
Doing so creates a `Transformer[In, Out]`.

 - **`Parser[In, Out]`** consumes a stream of `In` values, eventually producing an `Out`. Parsers are 
 - **`Transformer[In, Out]`** transfrorms a stream of `In` values to a stream of `Out` values. 
 - **`Splitter[In, Context]`** splits a stream of `In` events by selecting "substreams", 
   e.g. only the events associated with some child element in the XML, or for a specific JSON field.
   Each substream is identified by a `Context` value.
   By attaching a `Context => Parser[In, Out]` function to each substream, you can create a `Transformer[In, Out]`.

## Immutability

Instances of `Transformer`, `Parser`, and `Splitter` are *immutable*, meaning they can safely be
reused and shared at any time, even between multiple threads.

It's common to define an `implicit val fooParser: XmlParser[Foo] = /* ... */`

# Example

`XmlParser.attr("foo")` is a parser which will find the "foo" attribute of the first element it sees.

```xml
<!-- file: elem.xml -->
<elem foo="bar" />
```

```scala
val xml = JavaxSource.fromFile { new File("elem.xml") }
val elemFooParser: XmlParser[String] = XmlParser.attr("foo")
val result: String = elemFooParser.parse(xml)
assert(result == "bar")
```

Suppose you have some XML with a bunch of `<elem foo="..."/>` and you want the "foo" attribute from each of them.
This is a job for a Splitter. You write an `XmlSplitter` sort of like an XPATH, to describe how to get to each element that you want to parse.

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
val foos: List[String] = XmlSplitter
   .xml(* \ "elem")
   .joinBy(elemFooParser)
   .parseToList
   .parse(JavaxSource.fromFile { new File("root.xml") })
assert(foos == List("bar", "baz"))
```

Note that a `Splitter` has a handful of "attach a parser" methods.
The one you use will depend on whether the parser is available implicitly, and whether you care about the `Context` value for each substream.

 - `splitter.map(context => getParser(context))`
 - `splitter.joinBy(parser)`
 - `splitter.as[Out]`

# Running Parsers

A `Parser[In, Out]` allows several ways to consume a stream of data:

- `parse(Iterator[In]): Out` pulls from the iterator until a result is available, or the iterator ends
- `parse(Source[In]): Out` opens the source to obtain an iterator, pulls from the iterator to obtain a result, then closes the source
- `parseF(fs2.Stream[F, In]): F[Out]` (provided by an implicit from the `spac-interop-fs2` module) pulls from the stream until a result is available, or the stream ends, 
   with evaluation performed in the `F` context
- `.toPipe[F]: fs2.Pipe[F, In, Out]` (provided by an implicit from the `spac-interop-fs2` module) allows you to transform an fs2.Stream
- `.start` returns a low-level handler which can step through `In` values

## Basic Usage

```scala
// build.sbt
libraryDependencies += "io.dylemma.spac" %% "spac-core" % spacVersion

// Main.scala
import io.dylemma.spac._

val parser: Parser[In, Out] = /* ... */
val events: Iterator[In] = /* ... */
val result: Out = parser.parse(events)
```

## XML with Javax

```scala
// build.sbt
libraryDependencies ++= Seq(
   "io.dylemma" %% "xml-spac" % spacVersion,
   "io.dylemma" %% "xml-spac-javax" % spacVersion
)

// Main.scala
import io.dylemma.spac._
import io.dylemma.spac.xml._

val parser: XmlParser[Out] = /* ... */
val xml: Source[XmlEvent] = JavaxSource.fromFile(/* ... */)
val result: Out = parser.parse(xml)
```

## JSON with Jackson

```scala
// build.sbt
libraryDependencies ++= Seq(
   "io.dylemma" %% "json-spac" % spacVersion,
   "io.dylemma" %% "json-spac-jackson" % spacVersion
)

// Main.scala
import io.dylemma.spac._
import io.dylemma.spac.json._

val parser: JsonParser[Out] = /* ... */
val json: Source[JsonEvent] = JacksonSource.fromFile(/* ... */)
val result: Out = parser.parse(json)
```

### XML with fs2-data-xml

```scala
// build.sbt
libraryDependencies ++= Seq(
   "io.dylemma" %% "xml-spac" % spacVersion,
   "io.dylemma" %% "spac-interop-fs2" % spacVersion,
   "io.dylemma" %% "xml-spac-fs2-data" % spacVersion
)

// Main.scala
import cats.effect.IO
import io.dylemma.spac._
import io.dylemma.spac.xml._
import io.dylemma.spac.interop.fs2._

val parser: XmlParser[Out] = /* ... */
val rawXmlBytes: fs2.Stream[IO, Byte] = /* ... */
val xmlStream: fs2.Stream[IO, XmlEvent] = Fs2DataSource.fromRawXmlStream(rawXmlBytes)
val result: IO[Out] = parser.parseF(xmlStream)
```

### JSON with fs2-data-json

```scala
// build.sbt
libraryDependencies ++ Seq(
   "io.dylemma" %% "json-spac" % spacVersion,
   "io.dylemma" %% "spac-interop-fs2" % spacVersion,
   "io.dylemma" %% "json-spac-fs2-data" % spacVersion
)

// Main.scala
import cats.effect.IO
import io.dylemma.spac._
import io.dylemma.spac.json._
import io.dylemma.spac.interop.fs2._

val parser: JsonParser[Out] = /* ... */
val rawJsonBytes: fs2.Stream[IO, Byte] = /* ... */
val jsonStream: fs2.Stream[IO, JsonEvent] = Fs2DataSource.fromRawJsonStream(rawJsonBytes)
val result: IO[Out] = parser.parseF(jsonStream)
```

Convenience constructors for `Source[XmlEvent]` and `Source[JsonEvent]` are available from the `JavaxSource`
and `JacksonSource` objects, respectively. Those objects are considered "backends" that delegate to a separate
library for the low-level parsing (i.e. the JVM's built-in `javax.xml.stream` package in the case of `JavaxSource`,
and the `jackson-core` library in the case of `JacksonSource`). They are found in the `xml-spac-javax` and `json-spac-jackson`
modules respectively.

# Applying Transformers

A `Transformer[In, Out]` is applied by:

 - `.transform` to convert an `Iterator[In]` to an `Iterator[Out]`
 - `.transform` to convert a `Source[In]` to a `Source[Out]`
 - `.toPipe[F]` (provided by an implicit from the `spac-interop-fs2` module) to convert an `fs2.Stream[F, In]` to an `fs2.Stream[F, Out]`

# Under the Hood

`Parser` and `Transformer` both act as factories for their respective `Handler` traits.
Whenever you consume a stream with a Parser's `parse` method, or transform a stream with a Transformer,
a new `Handler` instance will be created and used to run the stream processing logic.

Handlers are internally-mutable, whereas the Parsers/Transformers that create them are not.

A Parser's `Handler` must respond to each input by either returning a result or a new handler representing the continuation of the parser logic.
Once the stream of inputs ends, the handler must produce a result. 
Since Handlers are internally-mutable, it's acceptable (and even preferable) for the Handler to simply update its internal state and return a reference to itself instead of constructing a new handler.

```scala
object Parser {
  trait Handler[-In, +Out] {
    def step(in: In): Either[Out, Handler[In, Out]]
    def finish(): Out
  }
}
```

A Transformer's `Handler` may respond to each input by any combination of; 
updating its internal state, 
emitting an output to a downstream handler, 
or signalling to the upstream that it no longer wants to receive any more inputs.
It may also choose to emit some additional outputs in response to the end of the stream of inputs.
It may also transform exceptions thrown by a downstream handler.

```scala
object Transformer {
  trait Handler[-In, +Out] {
    def push(in: In, out: HandlerWrite[Out]): Signal
    def finish(out: HandlerWrite[Out]): Unit
    def bubbleUp(err: Throwable): Nothing = throw err
  }
}
```
