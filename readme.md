XML SPaC
========

[![Build Status](https://travis-ci.org/dylemma/xml-stream.svg?branch=master)](https://travis-ci.org/dylemma/xml-stream)

**XML** **S**treaming **Pa**rser **C**ombinators is a Scala library for creating
[StAX](https://docs.oracle.com/javase/7/docs/api/javax/xml/stream/package-summary.html) parsers that are:

 - **Declarative** - You write *what* you want to get, not *how* to get it.
 - **Immutable** - Parsers that you create have no internal state.
 - **Composable** - Combine and transform parsers to handle complex data structures. 
 - **Fast** - With minimal abstraction to get in the way, speed rivals any hand-written handler.
 - **Streaming** - Parse huge XML documents without loading it all into memory.

There's a full-fledged example [further down](#writing-a-parser-by-example) in this readme,
and the [examples project](https://github.com/dylemma/xml-stream/tree/overhaul/examples/src/main/scala/io/dylemma/xml/example) has a collection of annotated examples,
but here's a taste of how you'd write a parser for a relatively-complex blog post XML structure:

```scala
val PostParser = Parser.combine(
	Parser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate),
	Splitter(* \ "author").first[Author],
	Splitter(* \ "stats").first[Stats],
	Splitter(* \ "body").first.asText,
	Splitter(* \ "comments" \ "comment").asListOf[Comment]
).as(Post)
```

# Get it!

Add the following to your `build.sbt` file:

```sbt
libraryDependencies += "io.dylemma" %% "xml-spac" % "0.2"
```

# How it Works

**XML SPaC** doesn't have any particular representation of a "stream". Instead, it defines the `ConsumableLike` typeclass:

```scala
trait ConsumableLike[-S, +In]{
	def apply[Out](source: S, handler: Handler[In, Out]
}
```

The `S` type parameter represents the "source" or the "stream".  
The `In` type parameter represents the events that the stream generates.
The `Out` type parameter represents the result type of the handler.

There are many different `ConsumableLike` instances already, including generalized ones for `Iterable` collections and
`Iterator`s, and XML-specific ones for `String`, `File`, and `InputStream`. If you have a more specific "Stream" type,
you can [write your own `ConsumableLike[StreamType, EventType]`].

A `Handler` is a mutable object that can accept inputs until it comes up with some result.

```scala
trait Handler[-In, +Out] {
	def isFinished: Boolean
	def handleInput(input: In): Option[Out]
	def handleError(err: Throwable): Option[Out]
	def handleEnd(): Out
}
```

The main classes in **XML SPaC** revolve around creating different `Handler`s.

## Consumer

A `Consumer[In, Out]` can create a `Handler[In, Out]` on demand. `Consumer`s have a `consume` method which will
consume a stream by creating a handler and running it until it emits a result.

## Parser

A `Parser[Context, Out]` creates a `Handler[XMLEvent, Try[Out]` when given a `Context`.  
**XML SPaC** has many helper methods for creating `Parser`s.  
`Parser`s have a `parse` method which will consume a stream of `XMLEvent`s to produce an `Try[Out]`.

## Transformer

A `Transformer[A, B]` turns a stream of `A` into a stream of `B`.  
A `Transformer[A, B]` can attach to a `Consumer[B, C]` to create a `Consumer[A, C]`.  
A `Transformer[A, B]` can attach to a `Transformer[B, C]` to create a `Transformer[A, C]`.

## Splitter

A `Splitter[In, Context]` combines with a `Parser[Context, Out]` to create a `Transformer[In, Out]`.  
It divides the incoming stream into "substreams", each of which have some "context" value, then runs the parser
on each of the substreams, passing the parser's result downstream.  
This is how you can write a `Parser` that only gets events from inside a particular element, like a `<comment>`.

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
