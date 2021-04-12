package io.dylemma.spac
package example

import cats.syntax.apply._
import io.dylemma.spac.xml._
import io.dylemma.spac.xml.JavaxSupport._

import scala.util.control.NonFatal

/** Demonstration of the 'spac trace' feature.
  * SpacExceptions thrown when running parsers will include contextual information
  * about the current input, caller, and any splitters that are currently "in play".
  */
object Example11_SpacException extends App {
	val rawXml =
		"""<root>
		  |   <thing>
		  |      <data id="hello">
		  |         <foo/>
		  |         <foo stuff="A" />
		  |         <bar id="B" />
		  |      </data>
		  |      <data id="123">
		  |         <foo stuff="C"/>
		  |      </data>
		  |   </thing>
		  |</root>""".stripMargin

	locally {
		val barParser = XmlParser.attr("id")
		val fooParser = XmlParser.attrOpt("stuff")
		val dataParser = (
			XmlParser.attr("id"),
			Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
			Splitter.xml("data" \ "bar").joinBy(barParser).parseFirstOpt,
		).tupled
		val rootParser = Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

		println("--first example (all good)")
		rootParser.parse(rawXml) foreach println
		/* Prints:
		(hello,List(None, Some(A)),Some(B))
		(123,List(Some(C)),None)
		 */
		println()
	}

	locally {
		val barParser = XmlParser.attr("id")
		val fooParser = XmlParser.attrOpt("stuff")
		val dataParser = (
			XmlParser.attr("id"),
			Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
			Splitter.xml("data" \ "bar").joinBy(barParser).parseFirst, // this will fail on the second <data> element since there's no <bar>
		).tupled
		val rootParser = Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

		println("--second example (missing first <bar>)")
		try rootParser.parse(rawXml)
		catch { case NonFatal(e) => e.printStackTrace() }
		/* Prints:
		io.dylemma.spac.SpacException$MissingFirstException: Parser context ended before the first String could be found.
			at Input(</data>) - {line: 11, col: 14, offset: 230}
			at Splitter(elem(data) \ elem(bar)) - Example11_SpacException.scala:47
			at Compound Parser member 3 of 3
			at InputContext(<data id="123">) - {line: 8, col: 22, offset: 151}
			at InputContext(<thing>) - {line: 2, col: 11, offset: 18}
			at InputContext(<root>) - {line: 1, col: 7, offset: 6}
			at Splitter(elem(root) \ elem(thing) \ elem(data)) - Example11_SpacException.scala:49
			at parse - Example11_SpacException.scala:52
		 */
		println()
	}

	locally {
		val barParser = XmlParser.attr("id")
		val fooParser = XmlParser.attrOpt("stuff")
		val dataParser = (
			XmlParser.attr("id").map(_.toInt), // will throw a NumberFormatException on `<data id="hello">`
			Splitter.xml("data" \ "foo").joinBy(fooParser).parseToList,
			Splitter.xml("data" \ "bar").joinBy(barParser).parseFirstOpt,
		).tupled
		val rootParser = Splitter.xml("root" \ "thing" \ "data").joinBy(dataParser).parseToList

		println("--third example (id.toInt exception)")
		try rootParser.parse(rawXml)
		catch { case NonFatal(e) => e.printStackTrace() }
		/* Prints:
		io.dylemma.spac.SpacException$CaughtError: Downstream logic error: java.lang.NumberFormatException: For input string: "hello"
			at Input(<data id="hello">) - {line: 3, col: 24, offset: 43}
			at Compound Parser member 1 of 3
			at InputContext(<data id="hello">) - {line: 3, col: 24, offset: 43}
			at InputContext(<thing>) - {line: 2, col: 11, offset: 18}
			at InputContext(<root>) - {line: 1, col: 7, offset: 6}
			at Splitter(elem(root) \ elem(thing) \ elem(data)) - Example11_SpacException.scala:76
			at parse - Example11_SpacException.scala:79
		Caused by: java.lang.NumberFormatException: For input string: "hello"
			at java.lang.NumberFormatException.forInputString(Unknown Source)
			at java.lang.Integer.parseInt(Unknown Source)
			at java.lang.Integer.parseInt(Unknown Source)
			at scala.collection.StringOps$.toInt$extension(StringOps.scala:910)
			at io.dylemma.spac.example.Example11_SpacException$.$anonfun$new$2(Example11_SpacException.scala:72)
			...
		 */
		println()
	}
}
