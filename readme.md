SPaC
====

[![Build Status](https://travis-ci.org/dylemma/xml-stream.svg?branch=master)](https://travis-ci.org/dylemma/xml-stream)

**S**treaming **Pa**rser **C**ombinators lets you create declarative, composable, high-performance handlers for iterative data.

It's especially good at making [StAX](https://docs.oracle.com/javase/7/docs/api/javax/xml/stream/package-summary.html) handlers,
allowing you to parse huge XML files without sacrificing speed, and without having to manage a bunch of internal state.

# Get it!

Add the following to your `build.sbt` file:

```sbt
libraryDependencies += "io.dylemma" %% "spac" % "0.2"
```

# Use it!

There's a full-fledged example [further down](#writing-a-parser-by-example) in this readme, but here's a taste of how you'd write a parser for a
relatively-complex blog post XML structure:

```scala
val PostParser = Parser.combine(
	Parser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate),
	Splitter(* \ "author").first[Author],
	Splitter(* \ "stats").first[Stats],
	Splitter(* \ "body").first.asText,
	Splitter(* \ "comments" \ "comment").asListOf[Comment]
).as(Post)
```

# The Old Ways

There are a few main approaches to parsing XML on the JVM, and they all have their ups and downs.
This library aims to have only "ups".

## DOM Parsing

The simplest approach for parsing XML is probably to load the entire document into memory (create a DOM), then query it
with XPaths or other similar operations. This approach is relatively quick and easy, but utterly falls apart for large
documents. The in-memory model of a DOM is typically upwards of four times the size of the raw XML, so you can quickly
run into OutOfMemoryErrors with larger files.

## JAXB

[JAXB](https://docs.oracle.com/javase/tutorial/jaxb/intro/) is like an ORM for XML. You feed it an XSD file and it will
auto-generate some Java sources that can be used to convert data back and forth between raw XML and POJOs. This can be
a time-saver in some cases, but the underlying implementation is very reflection-heavy (obfuscation of bytecode will
break it), and you don't always have access to XSD files for the XML you're trying to parse.

## SAX and StAX

The usual response to "DOM is taking too much memory" is to recommend using SAX or StAX. Both avoid loading an entire
document into memory, instead treating the XML as a series of events to be handled. The SAX approach "pushes" data to
a handler object, while the StAX approach acts as an iterator that needs to be "pulled" by client code. The downside to
these approaches is that for all but the simplest documents, the implementation of handlers can be fairly large. State
within the parsers is mutable by necessity, making it harder to follow, and harder to maintain. Common functionality
between parsers is typically very difficult to share, as it is usually closely tied to the state management.

# The New Way

**SPaC** treats XML as a stream of events, and makes it easy to define consumers for that stream. There are three
central concepts in the library: Parser, Splitter, and Transformer.

## Parser

A parser is an object that consumes a stream of XML events to produce some final result value.

For example, you could create a parser that collected `Characters` events from the stream, then concatenated their
values to produce a `String` as the result.

## Splitter

A splitter is an object that splits a stream of XML events into substreams.

For example, in a document with many `<foo>` elements, you could create a substream for all of the events inside
each individual `<foo>`.

## Transformer

A transformer is an object that turns a stream of XML events into a stream of results. Transformers are usually
created by combining a Splitter with a Parser.

For example, in the document with many `<foo>` elements, you could use a splitter to create substreams for the events
inside each individual `<foo>`, then use a Parser on each substream to create a `Bar` in memory. By doing this, you
will have transformed the XML stream into a `Bar` stream.

When you have a stream of results (e.g. `Bar`), you could run some side-effect on each element as it comes through,
collect them all to a list, or do something else entirely!

# Examples in Code

The [examples project](https://github.com/dylemma/xml-stream/tree/overhaul/examples/src/main/scala/io/dylemma/xml/example)
has a collection of small examples.

# Writing a Parser by Example

Let's write a parser for this pretend blog data file.

```xml
<blog>
  <post date="2015-11-16">
    <author name="dylemma" id="abc123"/>
    <stats likes="123" tweets="4"/>
    <body>Hello world!</body>
    <comments>
      <comment date="2015-11-18">
        <author name="anonymous" id="def456"/>
        <body>I'm commenting on your fake blog!</body>
      </comment>
    </comments>
  </post>
  <post date="2015-11-18">
    <author name="johndoe" id="004200"/>
    <stats likes="7" tweets="1"/>
    <body>A second blog post, huzzah!</body>
    <comments>
      <comment date="2015-11-19">
        <author name="anonymous" id="def456"/>
        <body>It's me again</body>
      </comment>
    </comments>
  </post>
</blog>
```

For this example, we'll use the following classes:

```scala
case class Post(date: LocalDate, author: Author, stats: Stats, body: String, comments: List[Comment])
case class Author(id: String, name: String)
case class Stats(numLikes: Int, numTweets: Int)
case class Comment(date: LocalDate, author: Author, body: String)
```

And the following imports:

```scala
import io.dylemma.spac._
```

### Parser[Author]

We'll start by defining a parser for the `Author` class:

```scala
implicit val AuthorParser: Parser[Any, Author] = Parser.combine(
	Parser.forMandatoryAttribute("id"),
	Parser.forMandatoryAttribute("name")
).as(Author)
```

What happened here is that we actually defined two parsers, then joined them together.
`forMandatoryAttribute("id")` is a parser that takes the "id" attribute from the first `StartElement` event it encounters.
Similarly, `forMandatoryAttribute("name")` is a parser that takes the "name" attribute.
In order to combine the "id" and the "name" parsers, we use `Parser.combine(...)` followed by `.as(Author)`.
This works because `Author` is a case class, and therefore the `Author` companion object can be treated as a
`(String, String) => Author`, which fits the signature of `.as`.
The `Any` in the type signature is the parser's context type. Some parsers require a context value in order to work.
This one doesn't require any particular context type.
We mark the `AuthorParser` as implicit so that it can be used with some of the parser convenience methods later on.

### Parser[Stats]

Building on the concepts from the `Author` parser, we can define the `Stats` parser.

```scala
implicit val StatsParser: Parser[Any, Stats] = Parser.combine(
	Parser.forMandatoryAttribute("likes").map(_.toInt),
	Parser.forMandatoryAttribute("tweets").map(_.toInt)
).as(Stats)
```

Note the addition of `map(_.toInt)`. A parser's `map` method transforms its result with the given function. Note that
if the function throws an exception, that exception will be caught and represented in the parser's result. Also note
that the `join` method is completely typesafe. Without the `map` calls, `as(Stats)` would fail since you would be
trying to pass Strings into a function that expected Ints.

### Parser[Comment]

Using some new concepts, we can define the `Comment` parser.

```scala
val commentDateFormat = DateTimeFormat.forPattern("yyyy-MM-dd")
implicit val CommentParser: Parser[Any, Comment] = Parser.combine(
	Parser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate),
	Splitter(* \ "author").first[Author],
	Splitter(* \ "body").first.asText
).as(Comment)
```

The `Splitter(* \ "author")` is a new Splitter that only passes along events coming from the `<author>` element directly within
the top-level element, which in this case could be anything. Then we call `first[Author]`, which implicitly looks for a
`Parser[Any, Author]` and should find our `AuthorParser` that we defined earlier. `first[T]` is actually shorthand for
`through(parser).parseFirst`, meaning that we will run the AuthorParser only on the first `<author>` substream, returning a
single `Author` instance rather than a list or option.

The `Splitter(* \ "body" ).first.asText` is a parser that collects the text in first `<body>` element directly within the top-level
element. Any `Characters` events encountered within that substream will be concatenated to generate the result.

### Parser[Post]

Combining the parsers and concepts from above, we can define the `Post` parser.

```scala
implicit val PostParser: Parser[Any, Post] = Parser.combine(
	Parser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate),
	Splitter(* \ "author").first[Author],
	Splitter(* \ "stats").first[Stats],
	Splitter(* \ "body").first.asText,
	Splitter(* \ "comments" \ "comment").asListOf[Comment]
).as(Post)
```

Note the use of `asListOf` instead of `first` for the comments. Instead of just taking the first `<comment>`, we gather all
of them into a list.

### Common Functionality is Easy to Share

Note the common functionality between `PostParser` and `CommentParser` for getting the `date` and `author`. That
functionality can be pulled out to some common location and reused anywhere, e.g.

```scala
val dateAttributeParser = (* % "date").map(commentDateFormat.parseLocalDate)
val authorElementParser = (* / "author").as[Author]

implicit val CommentParser: Parser[Comment] = (
  dateAttributeParser &
  authorElementParser &
  (* / "body" / Text)
).join(Comment)
```

## Using the Parser

Remember that the `<post>` element was inside the top-level `<blog>` element, so we can't run the `PostParser` directly
on the XML stream. We need to put it in the right context first.

```scala
val postTransformer: Transformer[XMLEvent, Post] = Splitter("blog" \ "post") through PostParser
// or
val postTransformerAlt: Transformer[XMLEvent, Post] = Splitter("blog" \ "post").as[Post]
```

The `postTransformer` is a `Transformer[XMLEvent, Post]`, meaning that it can be used to transform a stream of XMLEvent
to a stream of `Post` values. Depending on how you want to consume that stream, you might call `postTransformer.parseToList` or
`postTransformer.consumeForeach(println)`, or one of `Transformer`'s several other convenience methods. These methods return
`Parsers` or `Consumers`, both of which can then be used to consume the whole XML stream.

```scala
// the raw xml data can be a String, a File, an InputStream,
// or anything belonging to the `ConsumableLike` typeclass.
val xml = ...
postTransformer.consumeForEach(println) consume xml // println each Post as the stream finds it
println(postTransformer.consumeToList consume xml) // collect all of the Posts to a list
println(postTransformer.parseToList parse xml) // collect all of the Posts, but wrap parsing failures as values
```

You can use `parser.parse(xml)` or `consumer.consume(xml)`. `Parser` and `Consumer` are very similar, but the main
difference is that the output of a `Parser` will always be a `Try`.
Exceptions thrown by parsers' inner handlers will be caught and wrapped as `Failure`s. For example if the
`StatsParser` encountered a `"asdf"` as the value for the "likes" attribute, the `_.toInt` would throw an exception,
which would be caught, causing the result for that element to be an `Failure(NumberFormatException)`.

# Low Level Details

Under the hood, `Parser`, `Transformer`, and `Consumer` are all immutable factories for different kinds of `Handler`s.
A `Handler` is a *stateful* object that handles the stream data and eventually produces a result. If you use the
`parse` or `consume` convenience methods, you will never need to deal directly with a `Handler`.
