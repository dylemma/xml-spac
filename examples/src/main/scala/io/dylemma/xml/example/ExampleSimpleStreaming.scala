package io.dylemma.xml.example


import io.dylemma.xml.XMLEventEnumerator
import io.dylemma.xml.iteratee.ParsingDSL._
import play.api.libs.iteratee.Execution.Implicits.trampoline
import play.api.libs.iteratee.Iteratee

/**
 * Created by dylan on 10/11/2015.
 */
object ExampleSimpleStreaming {

	val libraryXml = """<library>
		| <book>Don Quixote</book>
		| <book>A Tale of Two Cities</book>
		| <book>The Lord of the Rings</book>
		| <book>The Little Prince</book>
		| <book>Harry Potter and the Sorcerer's Stone</book>
		| <book>And Then There Were None</book>
		|</library>""".stripMargin


	def main(args: Array[String]): Unit = {

		// Create an Enumerator of XMLEvents based on the `libraryXML` string
		val source = XMLEventEnumerator(libraryXml)

		// Create a parser similar to the one in `ExampleSimple`, but instead of
		// collecting results to a List, this one will `println` each Parser.Result
		// as it is encountered.
		val parser = (Root \ "library" \ "book").consumeText(Iteratee.foreach(println))

		// Run the parser on the XMLEvent Enumerator. The parser doesn't result in
		// any values (just side effects), so we don't look at the resulting future
		XMLEventEnumerator(libraryXml) |>>> parser.toIteratee

	}
}
