xml-stream
==========

This library leverages Play Framework's Iteratee library to create declarative, composable parsers for XML.

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
val consumer: AnyContextParser[List[Comment]] = (Root / "comments" / "comment").asList[Comment]

// on-the-fly handling via foreach
val consumer: AnyContextParser[Unit] = (Root / "comments" / "comment").foreach[Comment](println)
```

Then you run your consumer on an XML stream (`Enumerator[XMLEvent]`)

```scala
val stream = XMLEventEnumerator(() => openInputStream)
val result = stream |>>> consumer.toIteratee
```
