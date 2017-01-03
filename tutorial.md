Tutorial
========

<em>Note: the readme and tutorial documents currently reflect the upcoming 0.3-SNAPSHOT API. 
For documentation relevant to the latest release (v0.2), please refer to [the old readme](https://github.com/dylemma/xml-spac/blob/0.2/readme.md)</em>

---

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
implicit val AuthorParser: Parser[Any, Author] = (
	Parser.forMandatoryAttribute("id") and
	Parser.forMandatoryAttribute("name")
).as(Author)
```

What happened here is that we actually defined two parsers, then joined them together.
`forMandatoryAttribute("id")` is a parser that takes the "id" attribute from the first `StartElement` event it encounters.
Similarly, `forMandatoryAttribute("name")` is a parser that takes the "name" attribute.
We combine the "id" and "name" parsers using the `and` method (you could also use `~` if you prefer operators), 
then calling `.as(Author)` on the result.
This works because `Author` is a case class, and therefore the `Author` companion object can be treated as a
`(String, String) => Author`, which fits the signature of `.as`.
The `Any` in the type signature is the parser's context type. Some parsers require a context value in order to work.
This one doesn't require any particular context type.
We mark the `AuthorParser` as implicit so that it can be used with some of the parser convenience methods later on.

### Parser[Stats]

Building on the concepts from the `Author` parser, we can define the `Stats` parser.

```scala
implicit val StatsParser: Parser[Any, Stats] = (
	Parser.forMandatoryAttribute("likes").map(_.toInt) and
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
implicit val CommentParser: Parser[Any, Comment] = (
	Parser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate) and
	Splitter(* \ "author").first[Author] and
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
implicit val PostParser: Parser[Any, Post] = (
	Parser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate) and
	Splitter(* \ "author").first[Author] and
	Splitter(* \ "stats").first[Stats] and
	Splitter(* \ "body").first.asText and
	Splitter(* \ "comments" \ "comment").asListOf[Comment]
).as(Post)
```

Note the use of `asListOf` instead of `first` for the comments. Instead of just taking the first `<comment>`, we gather all
of them into a list.

### Common Functionality is Easy to Share

Note the common functionality between `PostParser` and `CommentParser` for getting the `date` and `author`. That
functionality can be pulled out to some common location and reused anywhere, e.g.

```scala
val dateAttributeParser = Parser.forMandatoryAttribute("date").map(commentDateFormat.parseLocalDate)
val authorElementParser = Splitter(* \ "author").first[Author]

implicit val CommentParser: Parser[Comment] = (
  dateAttributeParser and
  authorElementParser and
  Splitter(* \ "body").first.asText
).as(Comment)
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
