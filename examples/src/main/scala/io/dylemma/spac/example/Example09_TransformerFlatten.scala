package io.dylemma.spac
package example

import cats.syntax.apply._
import io.dylemma.spac.xml._
import io.dylemma.spac.xml.JavaxSupport._

object Example09_TransformerFlatten {

	/* In this example, we want to parse all of the names, including aliases,
	 * as a stream of Strings. To do this, we want to create a `Transformer[XMLEvent, String]`.
	 */
	val xml =
		"""<people>
		  |   <person alias="Bobby">Robert</person>
		  |   <person>Dylan</person>
		  |</people>
		""".stripMargin

	/* To start with, you'll typically create an XmlParser for the `<person>` element,
	 * and since a person can have an optional alias along with their normal name,
	 * the simplest way to model it is as an `XmlParser[List[String]]`.
	 */
	val personParser: XmlParser[List[String]] = (
		XmlParser.forText,
		XmlParser.forOptionalAttribute("alias")
	) mapN { (name, alias) =>
		name :: alias.toList
	}

	/* Then put the PersonParser in the context of the `<people>` element to get
	 * a `Transformer[XMLEvent, List[String]]`
	 */
	val namesListTransformer: Transformer[XmlEvent, List[String]] =
		Splitter.xml("people" \ "person").as(personParser)

	/* We could handle the stream of names via a `.parseForeach`, but that would be awkward. */
	val awkwardParser: XmlParser[Unit] = namesListTransformer.parseTap { names =>
		for(name <- names) println(s"Got a name: $name")
	}

	/* But we can do better by using the transformer's `.flatten` method first.
	 * This approach is also more friendly in case the <people> element appears
	 * inside some other elements and you want the list of names to be available
	 * in that context.
	 */
	val namesTransformer: Transformer[XmlEvent, String] =
		namesListTransformer.mapBatch { _.flatMap(Emit.fromSeq) }

	def main(args: Array[String]): Unit = {
		namesTransformer.parseTap(n => println(s"Got a name: $n")).parse(xml)
	}

}
