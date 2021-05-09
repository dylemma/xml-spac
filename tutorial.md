Tutorial
========

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
// import core classes like Parser and Splitter
import io.dylemma.spac._
// import xml-specific parser creation utilities
import io.dylemma.spac.xml._
// use java.xml.stream as the parser backend, allowing us to `.parse` Strings and Files
import io.dylemma.spac.xml.JavaxSupport._
// adds methods like mapN to tuples of parsers
import cats.syntax.apply._
```

The best approach to take when creating a complex parser is to start at the "bottom" of the DOM.
Create a simple parser that knows how to handle a simple element, then work your way up, creating more complex parsers in terms of the simpler parsers.

### XmlParser[Author]

We'll start by defining a parser for the `Author` class:

```xml
<!-- snippet from the xml above -->
<author name="johndoe" id="004200"/>
```

```scala
case class Author(id: String, name: String)

implicit val AuthorParser: XmlParser[Author] = (
  XmlParser.attr("id"),
  XmlParser.attr("name")
).mapN(Author)
```

What happened here is that we actually defined two parsers, then joined them together.
`attr("id")` is a parser that takes the "id" attribute from the first `ElemStart` event it encounters.
Similarly, `attr("name")` is a parser that takes the "name" attribute.

We combine the "id" and "name" parsers using 
The `mapN` method comes from the [Cats](https://typelevel.org/cats/) library when we did `import cats.syntax.apply._` since `Parser` is a member of Cats's [`Applicative`](https://typelevel.org/cats/typeclasses/applicative.html) typeclass.
This allows us to combine the "id" and "name" parsers to create a single parser that runs both of them and passes their outputs to the function we provide (`Author`).
Passing `Author` to `mapN` here is possible because `Author` is a case class, which makes its auto-generated companion object extend `(String, String) => Author`.
We mark the `AuthorParser` as implicit so that it can be used with some convenience methods later on.

Note that `mapN` is a method that Cats adds to tuples of any arity (up to 22 at the time of writing) in which each member is an instance of some `F[_]` type that belongs to the `Applicative` typeclass.
E.g. since `XmlParser` is applicative, you could do `(xmlParser1, xmlParser2, ..., xmlParserN).mapN { (r1, r2, ..., rN) => computeResult }`

### XmlParser[LocalDate]

Multiple different elements in the example XML above use a `date` attribute with the same format.

```xml
<!-- snippets from xml above -->
<post date="2015-11-16">
  ...
  <comment date="2015-11-18">
    ...
  </comment>
</post>
```

We can define a parser to extract the `date` attribute from an element, and reuse that parser to help define the parsers for those elements later.
To do this, start with an `XmlParser[String]` that extracts the `date` attribute, then `map` that parser using a `String => LocalDate` function to get an `XmlParser[LocalDate]`:

```scala
import java.time.LocalDate
import java.time.format.DateTimeFormatter

val commentDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd")
val dateAttributeParser: XmlParser[LocalDate] = XmlParser.attr("date").map(LocalDate.parse(_, commentDateFormat))
```

Note that if the function passed to a parser's `map` throws an exception while running that parser,
the exception will be caught, wrapped as a `SpacException.CaughtError`, and rethrown.
SPaC provides useful debugging information via the `SpacException` class, but ultimately it is up to you to properly handle edge cases and error conditions in your data and parser.

### XMLParser[Comment]

```xml
<!-- snippet from the xml above -->
<comment date="2015-11-18">
  <author name="anonymous" id="def456"/>
  <body>I'm commenting on your fake blog!</body>
</comment>
```

We want to parse a `<comment>` element as an instance of `Comment`. This will be done by combining three "inner" parsers:

- One parser to extract the `date` attribute from the `<comment>` element (we'll use the `dateAttributeParser` from earlier)
- One parser to extract an `Author` from the `<author>` child element
- One parser to extract the text from the `<body>` child element

If we were just trying to parse the `<body>` element in isolation, `XmlParser.forText` would do nicely.
But since we're trying to parse the *`<comment>`* element, and the `<body>` is a child element of that, we need to "relativize" the `forText` parser to the `<comment`> element.
We do this using a `Splitter`:

```scala
val commentBodyParser: XmlParser[String] = Splitter.xml(* \ "body").joinBy(XmlParser.forText).parseFirst
// or use a convenience method from XmlSplitter to replace the `joinBy` call
val commentBodyParser: XmlParser[String] = Splitter.xml(* \ "body").text.parseFirst
```

The `Splitter` and `joinBy` here is used to attach an `XmlParser.forText` to each `<body>` element that might appear inside the "first element" (which is represented by the `*`, i.e. the `<comment>` element in this example).
Since we only expect one `<body>` element to appear, we use `parseFirst` to collect only the text from the first `<body>` that appears.
If we had expected multiple `<body>` elements to appear inside a single `<comment>`, we could instead use `parseToList` to collect the text from each `<body>` as a `List[String]`.

We do something similar to handle the `<author>` element:

```scala
val commentAuthorParser: XmlParser[Author] = Splitter.xml(* \ "author").joinBy(AuthorParser).parseFirst
// or, since AuthorParser is available implicitly:
val commentAuthorParser: XmlParser[Author] = Splitter.xml(* \ "author").as[Author].parseFirst
```

We don't need to do the same thing for the `date` attribute, since that appears on the `<comment>` element itself.
Now we can combine these three parsers to create the `CommentParser`:

```scala
case class Comment(date: LocalDate, author: Author, body: String)

implicit val CommentParser: XmlParser[Comment] = (
  dateAttributeParser,
  Splitter.xml(* \ "author").as[Author].parseFirst,
  Splitter.xml(* \ "body").text.parseFirst
).mapN(Comment)
```

### XMLParser[Post]

To create the `Post` parser for a `<post>` element, we use the parsers and patterns shown above, combining them with the `mapN` method.
Technically we're missing a parser for the `<stats>` element, but it can be easily created similarly to how the `<author>` element's parser was created.

```xml
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
```

```scala
case class Stats(numLikes: Int, numTweets: Int)

implicit val StatsParser: XmlParser[Stats] = (
  XmlParser.attr("likes").map(_.toInt),
  XmlParser.attr("tweets").map(_.toInt)
).mapN(Stats)

case class Post(date: LocalDate, author: Author, stats: Stats, body: String, comments: List[Comment])

implicit val PostParser: XmlParser[Post] = (
  dateAttributeParser,
  Splitter.xml(* \ "author").as[Author].parseFirst,
  Splitter.xml(* \ "stats").as[Stats].parseFirst,
  Splitter.xml(* \ "body").text.parseFirst,
  Splitter.xml(* \ "comments" \ "comment").as[Comment].parseToList
).mapN(Post)
```

Note the use of `parseToList` instead of `parseFirst` for the comments. 
Instead of just taking the first `<comment>`, we gather all of them into a list.

## Using the Parser

Now that we have a parser for the `<post>` element, we're just about done.
The entire XML document is a single `<blog>` element that contains multiple `<post>` elements.

At this point we have a choice: 

1. Create a Parser that consumes the entire document, interpreting each `<post>` element to build a `List[Post]`
2. Create a Transformer that allows us to treat the document as a stream of `Post` values, emitting one for each `<post>` element

In either approach, we need to "relativize" the `<post>` parser to the `<blog>` element:

```xml
<blog>
  <post>...</post>
  <post>...</post>
</blog>
```

```scala
val postTransformer: XmlTransformer[Post] = Splitter.xml("blog" \ "post").as[Post]
```

Note that in previous examples, we used `*` to represent the "current element".
We could have done that here, but it can be sometimes be helpful to see the actual name of the top-level element when defining the top-level parser.

If you want to treat the document as a stream of Posts, obtain a `fs2.Stream` or `Iterator` of `XmlEvent`
(generally by using one of the utility methods on `JavaxSource` or `Fs2DataSource`),
then use Transformer's `toPipe` or `transform` method to convert it.

```scala
import cats.effect.SyncIO

val blogFile = new File("./blog.xml")

// use an fs2 Stream, transformed via `postTransformer.toPipe`
JavaxSource
   .syncIO(blogFile) // Stream[SyncIO, XmlEvent]
   .through(postTransformer.toPipe) // Stream[SyncIO, Post]
   .foreach(post => SyncIO { println(s"Output: $post") }) // Stream[SyncIO, Nothing]
   .compile // intermediate object
   .drain // SyncIO[Unit]
   .unsafeRunSync() // Unit

// obtain an iterator of events wrapped as a Resource
JavaxSource
   .syncIO // intermediate object
   .iteratorResource(blogFile) // Resource[SyncIO, Iterator[XmlEvent]]
   .map(postTransformer.transform) // Resource[SyncIO, Iterator[Post]]
   .use(postItr => SyncIO {
      postItr.foreach { post => println(s"Output: $post") }
   }) // SyncIO[Unit]
   .unsafeRunSync() // Unit
```

If you want to build a list of all of the Posts, use Parser's `parse` or `parseF` method.

```scala
val blogParser: XmlParser[List[Post]] = postTransformer.parseToList
val blogFile = new File("./blog.xml")

// synchronous execution
val allPosts: List[Post] = blogParser.parse(blogFile)

// deferred execution
val readAllPosts: SyncIO[List[Post]] = blogParser.parseF[SyncIO, File](blogFile)
```

Note that the `JavaxSource` utility methods and the `parse/parseF` methods are generic, using typeclasses to support multiple different source types.
The examples above used `File` as the source, but it could've just as easily used `String` or `Resource[F, InputStream]` or one of several other sources.
