XML SPaC
========

> *Note: the main branch currently has yet-to-be-released code for the upcoming 0.9 release. See [here](https://github.com/dylemma/xml-spac/tree/0.8) to view the code and readme from the latest release*

**API Docs:** [spac-core](https://javadoc.io/doc/io.dylemma/spac-core_2.13/0.8/io/dylemma/spac/index.html) | [xml-spac](https://javadoc.io/doc/io.dylemma/xml-spac_2.13/0.8/io/dylemma/spac/xml/index.html) | [json-spac](https://javadoc.io/doc/io.dylemma/json-spac_2.13/0.8/io/dylemma/spac/json/index.html)

**S**treaming **Pa**rser **C**ombinators is a Scala library for building stream consumers in a declarative style, specialized for tree-like data types like XML and JSON.

It delegates to a backend of your choice to obtain a stream of events:

 - **XML** support via:
   - [javax.xml.stream](https://docs.oracle.com/javase/8/docs/api/javax/xml/stream/package-summary.html), a.k.a. "StAX"
   - [fs2-data-xml](https://fs2-data.gnieh.org/)
 - **JSON** support via:
   - [Jackson](https://github.com/FasterXML/jackson-core)
   - [fs2-data-json](https://fs2-data.gnieh.org/)

And provides you the ability to create event consumers that are:

 - **Declarative** - You write *what* you want to get, not *how* to get it
 - **Immutable** - Parsers may be shared and reused without worry
 - **Composable** - Combine and transform parsers to handle complex data structures
 - **Fast** - With minimal abstraction to get in the way, speed rivals any hand-written handler
 - **Streaming** - Parse huge XML/JSON documents from events, not a DOM

You can jump into a full [tutorial](tutorial.md), or check out the [examples](examples/src/main/scala/io/dylemma/xml/example),
but here's a taste of how you'd write a parser for a relatively-complex blog post XML structure:

```scala
val PostParser = (
  XmlParser.attr("date").map(LocalDate.parse(_, commentDateFormat)),
  Splitter.xml(* \ "author").as[Author].parseFirst,
  Splitter.xml(* \ "stats").as[Stats].parseFirst,
  Splitter.xml(* \ "body").text.parseFirst,
  Splitter.xml(* \ "comments" \ "comment").as[Comment].parseToList
).mapN(Post)
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

 - **`Parser[In, Out]`** consumes a stream of `In` values, eventually producing an `Out`. Parsers are [`Applicative`](https://typelevel.org/cats/typeclasses/applicative.html)
 - **`Transformer[In, Out]`** transfrorms a stream of `In` values to a stream of `Out` values. 
 - **`Splitter[In, Context]`** splits a stream of `In` events by selecting "substreams", 
   e.g. only the events associated with some child element in the XML, or for a specific JSON field.
   Each substream is identified by a `Context` value.
   By attaching a `Context => Parser[In, Out]` function to each substream, you can create a `Transformer[In, Out]`.

Instances of `Transformer`, `Parser`, and `Splitter` are *immutable*, meaning they can safely be
reused and shared at any time, even between multiple threads.
It's common to define an `implicit val fooParser: XmlParser[Foo] = /* ... */`

## Example

`XMLParser.attr("foo")` is a parser which will find the "foo" attribute of the first element it sees.

```xml
<!-- file: elem.xml -->
<elem foo="bar" />
```

```scala
val xml = new File("elem.xml")
val elemFooParser: XmlParser[String] = XMLParser.attr("foo")
val result: String = elemFooParser.parse(xml)
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
val splitter: XmlSplitter[Unit] = Splitter.xml(* \ "elem")
val transformer: XmlTransformer[String] = splitter.joinBy(elemFooParser)

val rootParser: XmlParser[List[String]] = transformer.parseToList
val root: List[String] = rootParser.parse(xml)
assert(root == List("bar", "baz"))
```

Note that a `Splitter` has a handful of "attach a parser" methods.
The one you use will depend on whether the parser is available implicitly, and whether you care about the `Context` value for each substream.

 - `splitter.map(context => getParser(context))`
 - `splitter.joinBy(parser)`
 - `splitter.as[Out]`

> TODO: change this link to a live javadoc.io link for the ContextMatcherSyntax once 0.9 is released

Check out [the docs](#link-todo) for more ways to construct XmlSplitters.

# Running Parsers

To run a `Parser[In, Out]`, use either its `parse` or `parseF` method.

These methods accept any source that belongs to the `Parsable` typeclass,
where `parse` uses blocking operations (more suitable for when the source is a `String` or a `java.io.Reader/InputStream`),
and `parseF` suspends the evaluation in an `F[_]` effect type.

The following source types are supported by default:

- `Iterable[In]`
- `cats.data.Chain[In]`,
- `fs2.Stream[F, In]` (as long as `F` belongs to the `cats.effect.Sync` typeclass, or `F = fs2.Fallible`)
  
Additional source types are supported via imports from a specific "support module",
where you choose a "parser backend" to wrap/convert the underlying source to a stream that the SPaC parser can understand:

- `String` (provided by the various support modules) i.e. some raw XML or JSON
- `java.io.File` (provided by the Javax/Jackson support modules for XML/JSON respectively)
- `cats.effect.Resource[F, java.io.InputStream]` (provided by the Javax/Jackson support modules for XML/JSON respectively)
- `cats.effect.Resource[F, java.io.Reader]` (provided by the Javax/Jackson support modules for XML/JSON respectively)
- `fs2.Stream[F, Char]` (provided by the fs2-data support modules)
- `fs2.Stream[F, Byte]` (provided by the fs2-data support modules, as long as you import an appropriate implicit from [`fs2.data.text`](https://fs2-data.gnieh.org/api/fs2/data/text/index.html))
- `fs2.Stream[F, fs2.data.xml.XmlEvent]` (provided by the fs2-data-xml support module)
- `fs2.Stream[F, fs2.data.json.Token]` (provided by the fs2-data-json support module)

Note that the "support modules" are expressed in code as a pair of objects with the naming convention `FooSupport` and `FooSource`,
where `FooSupport` defines implicits that contribute to the `Parsable` typeclass,
and `FooSource` is a utility for constructing `fs2.Stream[F, io.dylemma.spac.xml.XmlEvent]` or `fs2.Stream[F, io.dylemma.spac.json.JsonEvent]` from some underlying source.

# Applying Transformers

A `Transformer[In, Out]` is applied by:

 - `.transform` to convert an `Iterator[In]` to an `Iterator[Out]`
 - `.toPipe[F]` to convert an `fs2.Stream[F, In]` to an `fs2.Stream[F, Out]`

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
