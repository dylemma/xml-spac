xml-stream
==========

This library lets you create declarative, composable parsers that work on streaming XML,
without all of the boilerplate that comes with SAX or StAX. The syntax for creating parsers is similar to that of
[play-json's `Reads`](https://www.playframework.com/documentation/2.5.x/ScalaJsonCombinators#Putting-it-all-together)

```scala
case class Comment(date: String, user: User, stats: Stats, body: String)

implicit val CommentParser: AnyContextParser[Comment] = (
  (* % "date") &             // get the "date" attribute from the main element
  (* / "user").as[User] &    // extract the <user> child element as a User
  (* / "stats").as[Stats] &  // extract the <stats> child element as a Stats
  (* / "body" % Text)        // get the text from the <body> child element
).join(Comment)              // combine the four results into a Comment
```

Once you define a parser for a type, you can use it to consume an XML stream. 
You can handle results on-the-fly, or collect them to a List/Option/single item.

```scala
// collect results to a List
val parser1: AnyContextParser[List[Comment]] = 
  (Root / "comments" / "comment").asList[Comment]

// on-the-fly handling via foreach
val parser2: AnyContextParser[Unit] = 
  (Root / "comments" / "comment").foreach[Comment](println)
```

Then you run your consumer on an XML stream (`Enumerator[XMLEvent]`)

```scala
val stream = ... // e.g. an InputStream, File, String
val result: Future[Parser.Result[Unit]] = parser2 parse stream 
```

Check out [the examples](tree/master/examples/src/main/scala/io/dylemma/xml/example) for more!