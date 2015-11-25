package io.dylemma.xml.example

import io.dylemma.xml.Result.Success
import io.dylemma.xml._
import ParsingDSL._
import play.api.libs.iteratee.Execution.Implicits.trampoline

/**
 * Created by dylan on 11/24/2015.
 */
object MultiplexExample extends App {

	val rawXml = """<stuff>
		|  <text>Hello world</text>
		|  <br/>
		|  <otherStuff>Hello!</otherStuff>
		|  <code>  &lt;HTML&gt;
		|  &lt;BODY&gt;
		|    Hello, John Smith
		|  &lt;/BODY&gt;
		|&lt;/HTML&gt;
		|</code>
		|  <br/>
		|  <floop>doop</floop>
		|  <doop/>
		|  <br/>
		|  <text>Here's some more text, hooray!</text>
		|  <br/>
		|</stuff>""".stripMargin

	// Define a model for the <text> <br> and <code> elements
	sealed trait HtmlThing
	case class TextBlock(s: String) extends HtmlThing
	case object Br extends HtmlThing
	case class CodeBlock(s: String) extends HtmlThing

	// this context matcher will succeed on <text>, <br>, and <code> elements,
	// and return the tag name as context
	val htmlContext = ("text" | "br" | "code").extractName

	/* This parser requires a `String` context, and will delegate to a
	 * different internal parser depending on the context's value.
	 * Note that if a different context e.g. "foo" is passed to this particular
	 * parser, it'll give off an Error result with a MatchError inside.
	 */
	val htmlParser = Parser.multiplex[String] {
		case "text" => (* % Text).map(TextBlock(_))
		case "br" => Parser.done(Success(Br))
		case "code" => (* % Text).map(CodeBlock(_))
	}

	// put everything together
	println("Proper wiring:")
	val consumer = (Root / "stuff" / htmlContext).through(htmlParser).foreach(println)
	consumer.parse(rawXml)

	println("\n\nAnd now with match errors:")
	val consumer2 = (Root / "stuff" / *.extractName).through(htmlParser).foreachResult(println)
	consumer2.parse(rawXml)
}
