package io.dylemma.xml.example

import io.dylemma.xml.XMLEventEnumerator
import io.dylemma.xml.iteratee.ParsingDSL._
import play.api.libs.iteratee.Execution.Implicits.trampoline

/**
 * Created by dylan on 10/11/2015.
 */
object ExampleSimple {

	val libraryXml = """<library>
		| <book>Don Quixote</book>
		| <book>A Tale of Two Cities</book>
		| <book>The Lord of the Rings</book>
		| <book>The Little Prince</book>
		| <book>Harry Potter and the Sorcerer's Stone</book>
		| <book>And Then There Were None</book>
		|</library>""".stripMargin

	val parser = (Root / "library" / "book" % Text.asList)

	def main(args: Array[String]): Unit = {

		// Create an Enumerator of XMLEvents based on the `libraryXML` string
		val source = XMLEventEnumerator(libraryXml)

		// Run the parser on the XMLEvent Enumerator, returning a Future
		val parseResultFuture = XMLEventEnumerator(libraryXml) |>>> parser.toIteratee()

		// Since we're using play's `trampoline` ExecutionContext, the Future will
		// have run in the current thread, so we don't need to wait for it.
		val parseResult = parseResultFuture.value.get.get

		// The `parseResult` is a Parser.Result containing the list of titles
		parseResult match {
			case Parser.Error(cause) => cause.printStackTrace()
			case Parser.Empty => println("no results")
			case Parser.Success(titles) =>
				for (title <- titles) println(s"book: $title")
		}

		// you can also use `foreach` with Parser.Results
		for {
			titles <- parseResult
			title <- titles
		} println(s"book foreach: $title")

	}
}
