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
  <!-- Continue ad-infinitum -->
</blog>
```

We'll represent that data in blog with these classes, ultimately treating the blog as a series of `Post`.

```scala
case class Post(date: LocalDate, author: Author, stats: Stats, body: String, comments: List[Comment])
case class Author(id: String, name: String)
case class Stats(numLikes: Int, numTweets: Int)
case class Comment(date: LocalDate, author: Author, body: String)
```

# Setup

Start by adding the library dependencies.
In this example we use `xml-spac-javax` to get access to the `JavaxSource` helper,
which delegates the low-level parsing to the JVM's native `javax.xml.stream` classes.
Note that including `xml-spac-javax` should cause the others to be included transitively,
so you don't necessarily have to type out all four.

```scala
// build.sbt
libraryDependencies ++= Seq(
   "io.dylemma" %% "spac-core" % spacVersion,      // base classes like Parser and Transformer
   "io.dylemma" %% "xml-spac" % spacVersion,       // xml-specific Parser support
   "io.dylemma" %% "xml-spac-javax" % spacVersion, // javax integration
   "org.typelevel" %% "cats-core" % catsVersion    // helpers like `.mapN`
)
```

Add the following imports:

```scala
import io.dylemma.spac._     // for Parser | Transformer | Splitter | Source
import io.dylemma.spac.xml._ // for XmlParser | Splitter.xml
import cats.syntax.apply._   // for `.mapN`
```

# Writing Parsers

The best approach to take when creating a complex parser is to start at the "bottom" of the DOM.
Create a simple parser that knows how to handle a simple element, then work your way up, creating more complex parsers in terms of the simpler parsers.

## XmlParser[Author]

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
).mapN(Author.apply)
```

What happened here is that we actually defined two parsers, then joined them together.
`attr("id")` is a parser that takes the "id" attribute from the first `ElemStart` event it encounters.
Similarly, `attr("name")` is a parser that takes the "name" attribute.

We combine the "id" and "name" parsers using 
The `mapN` method comes from the [Cats](https://typelevel.org/cats/) library when we did `import cats.syntax.apply._` since `Parser` is a member of Cats's [`Applicative`](https://typelevel.org/cats/typeclasses/applicative.html) typeclass.
This allows us to combine the "id" and "name" parsers to create a single parser that runs both of them and passes their outputs to the function we provide (`Author.apply`).
(Note that in Scala 2.x we could've just passed `Author` since it is the companion to a case class and so it extends `(String, String) => Author`, but in Scala 3 you'll get a warning about auto-insertion of the `apply` method).
We mark the `AuthorParser` as implicit so that it can be used with some convenience methods later on.

Note that `mapN` is a method that Cats adds to tuples of any arity (up to 22 at the time of writing) in which each member is an instance of some `F[_]` type that belongs to the `Applicative` typeclass.
E.g. since `XmlParser` is applicative, you could do `(xmlParser1, xmlParser2, ..., xmlParserN).mapN { (r1, r2, ..., rN) => computeResult }`

## XmlParser[LocalDate]

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

## XMLParser[Comment]

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
).mapN(Comment.apply)
```

## XMLParser[Post]

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
).mapN(Stats.apply)

case class Post(date: LocalDate, author: Author, stats: Stats, body: String, comments: List[Comment])

implicit val PostParser: XmlParser[Post] = (
  dateAttributeParser,
  Splitter.xml(* \ "author").as[Author].parseFirst,
  Splitter.xml(* \ "stats").as[Stats].parseFirst,
  Splitter.xml(* \ "body").text.parseFirst,
  Splitter.xml(* \ "comments" \ "comment").as[Comment].parseToList
).mapN(Post.apply)
```

Note the use of `parseToList` instead of `parseFirst` for the comments. 
Instead of just taking the first `<comment>`, we gather all of them into a list.

## Transformer[Post]

Now that we have a parser for the `<post>` element, we're just about done.
The entire XML document is a single `<blog>` element that contains multiple `<post>` elements.
If we tried to run `PostParser` on the entire file, it would fail, since it's expecting a `<post>` as the top-level element.
To treat the file as a series of `Post`, we use `Splitter` again to select the context of the `<post>` elements. 

```xml
<blog>
  <post>...</post>
  <post>...</post>
</blog>
```

```scala
val postTransformer: XmlTransformer[Post] = Splitter.xml("blog" \ "post").as[Post]
```
*Note that in previous examples, we used `*` to represent the "current element".
We could have done that here, but it can be sometimes be helpful to see the actual name of the top-level element when defining the top-level parser.*

Now, we can decide how to consume that series of posts by calling one of `postTransformer`'s `parse*` methods:

```scala
val postPrinter: XmlParser[Unit] = postTransformer.parseTap(println)
val postCollector: XmlParser[List[Post]] = postTransformer.parseToList
```

These parsers are ready to handle the entire blog file.

# Using Parsers

Until this point we've only declared *how* we want to handle data; now it's time to actually *handle* data.

Parsers operate on streams of data; an XmlParser consumes a stream of `XmlEvent`. 
To get that stream, you need to convert your raw XML into a `Source[XmlEvent]`.
That's what the `xml-spac-javax` library is for; it provides the `JavaxSource` object which contains helpers to construct a `Source[XmlEvent]` from various underlying data types.

If you want to treat the document as a stream of Posts, obtain a `fs2.Stream` or `Iterator` of `XmlEvent`
(generally by using one of the utility methods on `JavaxSource` or `Fs2DataSource`),
then use Transformer's `toPipe` or `transform` method to convert it.

```scala
val blogFile = new File("./blog.xml")
val blogSource = JavaxSource.fromFile(blogFile)

postPrinter.parse(blogFile)
// or
val posts: List[Post] = postCollector.parse(blogFile)
```

Congratulations on reaching the end of the tutorial!
For even more information on how to use xml-spac, check out [the examples](./examples).
