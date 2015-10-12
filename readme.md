xml-stream
==========

This library leverages Play Framework's Iteratee library to create declarative, composable parsers for XML.

```scala
case class Comment(date: String, user: User, stats: Stats, body: String)

implicit val CommentParser: Parser[Comment] = (
  (Elem % "date") ~             // get the "date" attribute
  (Elem \ "user").as[User] ~    // extract the <user> element as a User
  (Elem \ "stats").as[Stats] ~  // extract the <stats> element as a Stats
  (Elem \ "body" \ Text)        // get the text from the <body> element
)(Comment.apply _)              // combine the four results into a Comment
```

Once you define a parser for a type, you can use it to consume an XML stream. 
You can handle results on-the-fly, or collect them to a List/Option/single item.

```scala
// collect results to a List
val consumer: Parser[List[Comment]] = (Root \ "comments" \ "comment").asList[Comment]

// on-the-fly handling via Iteratee.foreach
val consumer: Parser[Nothing] = (Root \ "comments" \ "comment").consumeAs[Comment](
  Iteratee.foreach(println)
)
```

Then you run your consumer on an XML stream (`Enumerator[XMLEvent]`)

```scala
val stream = XMLEventEnumerator(() => openInputStream)
val result = stream |>>> consumer.toIteratee
```
