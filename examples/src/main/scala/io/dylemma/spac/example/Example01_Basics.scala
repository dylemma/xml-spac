package io.dylemma.spac.example

import io.dylemma.spac._
import io.dylemma.spac.xml._

import scala.util.Try

object Example01_Basics extends App {

	/* Parsers work by consuming "tokens". I.e. an `XmlParser` consumes `XmlEvent`s
	 * and a `JsonParser` consumes `JsonEvent`s. Converting raw data into a stream
	 * of tokens is not the focus of this library, but helpers are provided to
	 * delegate to other libraries to accomplish the conversion.
	 *
	 * The simplest way to use a parser is to call its `parse` method, which expects
	 * a `Source` of the parser's `In` type. A `Source[A]` provides an `Iterator[A]`
	 * and a close function, so it can be used to represent resources like File handles
	 * which must be closed after being consumed.
	 *
	 * To get a `Source[XmlEvent]`, you can use the helpers from `JavaxSource`, which
	 * is provided by the `"io.dylemma" %% "xml-spac-javax"` dependency. The `JavaxSource`
	 * helpers delegate to the `javax.xml.stream` classes from Java to handle the
	 * underlying parsing.
	 */
	val libraryXml: Source[XmlEvent] = JavaxSource.fromString("""<library>
		| <book>Don Quixote</book>
		| <book>A Tale of Two Cities</book>
		| <book>The Lord of the Rings</book>
		| <book>The Little Prince</book>
		| <book>Harry Potter and the Sorcerer's Stone</book>
		| <book>And Then There Were None</book>
		|</library>""".stripMargin)

	/*
	An `XmlParser[Out]` is able to parse a stream of XMLEvents to produce an `Out`.

	The `XmlParser.forText` parser will collect all of the `Characters` events it encounters, and concatenate them.
	 */
	val bookParser: XmlParser[String] = XmlParser.forText

	/*
	If we run the `bookParser` by itself on the `libraryXml`, we get the titles of all of the
	books, along with the whitespace between the <book> elements. We probably don't want that...
	 */
	val allBookTitles = bookParser parse libraryXml
	println(allBookTitles)
	println("\n")

	/*
	We probably want the titles of each book separately. To do this, we use a `Splitter`.
	A `Splitter` divides the incoming stream of events into substreams. This one will make
	a new substream for each `<book>` element inside a `<library>` element, i.e. there
	will be a substream for `"<book>Don Quixote</book>"` and another substream for
	`"<book>A Tale of Two Cities</book>"`, and so on.

	Note that some `Splitters` can extract a "context" value. This one simply matches without
	extracting anything, so its type parameter is just `Unit`.
	 */
	val bookSplitter: XmlSplitter[Unit] = Splitter.xml("library" \ "book")

	/*
	By attaching a parser to a splitter, you run the parser on each individual substream.
	This way we can get a separate event for each book.
	The result of combining a Splitter and an XmlParser is called a "Transformer" because it
	"transforms" an stream of inputs into a stream of something else.
	*/
	val bookTransformer: Transformer[XmlEvent, String] = bookSplitter.joinBy(bookParser)

	/*
	To actually get a result from a stream, you'll either need an `XmlParser`.

	Transformers can be turned into Parsers via a handful of convenience methods.
	 */
	val bookListParser: XmlParser[List[String]] = bookTransformer.parseToList

	/*
	The underlying handler created by a Parser may throw exceptions when handling inputs.
	Normally these exceptions will bubble up to whatever method invoked the consumer. In
	cases where these errors need to be caught, you can use the `wrapSafe` method on a Consumer.
	This will wrap its output in a `scala.util.Try` class, where exceptions will appear as
	`Failure` instances, and regular outputs will appear inside `Success` instances.
	This is particularly useful when the Parser is being used as a component of another Parser.
	 */
	val safeBookListConsumer: XmlParser[Try[List[String]]] = bookListParser.wrapSafe

	/*
	The bookList parser and consumer will yield the same result; the list of titles emitted by the `bookTransformer`.
	 */
	val allBooksResult1 = bookListParser parse libraryXml
	println(allBooksResult1)
	println("\n")

	/*
	You can handle results as they are discovered by using one of the `foreach` transformer methods.
	 */
	val foreachConsumer = bookTransformer.parseTap{ title => println(s"book: $title") }
	foreachConsumer parse libraryXml

	/*
	You could also use the Transformer's `transform` method directly on the `Iterator[XmlEvent]`
	 */
	libraryXml.iterateWith { xmlEvents =>
		println("---")
		val bookIterator: Iterator[String] = bookTransformer.transform(xmlEvents)
		bookIterator.foreach(println)
	}

}
