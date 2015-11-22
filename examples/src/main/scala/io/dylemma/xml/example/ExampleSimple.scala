package io.dylemma.xml.example

import io.dylemma.xml.ParsingDSL._
import io.dylemma.xml.Result
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

		// Parsers can parse anything belonging to the `AsInputStream` type class,
		// e.g. Strings, InputStreams, and Files
		val parseResultFuture = parser parse libraryXml

		// Since we're using play's `trampoline` ExecutionContext, the Future will
		// have run in the current thread, so we don't need to wait for it.
		val parseResult = parseResultFuture.value.get.get

		// The `parseResult` is a Parser.Result containing the list of titles
		parseResult match {
			case Result.Error(cause) => cause.printStackTrace()
			case Result.Empty => println("no results")
			case Result.Success(titles) =>
				for (title <- titles) println(s"book: $title")
		}

		// you can also use `foreach` with Parser.Results
		for {
			titles <- parseResult
			title <- titles
		} println(s"book foreach: $title")

	}
}
