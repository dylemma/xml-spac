package io.dylemma.xml.example

import javax.xml.stream.events.XMLEvent

import io.dylemma.spac._
import io.dylemma.spac.xml._

object Example6_NestedContexts extends App {

	/** This Data class requires an `info` and `context` value to be passed in from
	  * further up the stack. BUT, for argument's sake let's assume we have MANY
	  * `<data>` elements in our XML, so we want to make a `Transformer[XMLEvent, Data]`
	  * for the whole document.
	  *
	  * @param info
	  * @param context
	  * @param value
	  */
	case class Data(info: String, context: String, value: Int)

	/* We want to parse this `xml` as a stream of `Data` instances,
	 * without resorting to collecting a List[Data] for each <thing>.
	 */
	val xml =
		"""<stuff>
		  |  <info id="DYLAN"/>
		  |  <thing>
		  |    <context id="A"/>
		  |    <data>123</data>
		  |    <data>124</data>
		  |    <data>125</data>
		  |  </thing>
		  |  <thing>
		  |    <context id="B"/>
		  |    <data>567</data>
		  |    <data>568</data>
		  |    <data>569</data>
		  |  </thing>
		  |</stuff>
		""".stripMargin

	// We'll define some standalone parsers/transformers ahead of time...

	/** Parses the "id" attribute from the first `<info>` in a `<stuff>` */
	val stuffInfoParser = XMLSplitter("stuff" \ "info").first.attr("id")

	/** Get the "id" attribute from the first <context> element in a <thing> */
	val thingContextParser = XMLSplitter("thing" \ "context").first.attr("id")

	/** Get a stream of each `<data>`'s Int values from inside a <thing> */
	val thingDataTransform = XMLSplitter("thing" \ "data").asText.map(_.toInt)

	/** Creates a stream of Data objects from within a `<stuff>` */
	val verboseSelectDataFromStuff: Transformer[XMLEvent, Data] = {
		// get the <info> element's "id" attribute, using it to create an inner transformer
		stuffInfoParser.followedByStream { infoId =>
			println(s"Captured info.id as $infoId")

			// Select into the <thing> elements, using `flatMap` to attach an inner transformer to each one.
			XMLSplitter("stuff" \ "thing").flatMap { ignoredContext =>
				/* This block will run once per <thing>.
				 *
				 * Note: We could pass a Transformer instead of a function, since Transformer counts
				 * as a function that returns itself. But doing so would make the println no longer be
				 * part of the function passed to flatMap, and our output would be less interesting!
				 */
				println("Entered a <thing>")

				// capture the "context.id"
				thingContextParser.followedByStream { contextId =>
					println(s"Captured context.id as $contextId")

					// Now it's possible to create a stream of Data objects from within the <thing>,
					// since we have access to the contexts passed from above
					thingDataTransform.map { value =>
						Data(infoId, contextId, value)
					}
				}
			}
		}
	}

	// Same as above, but using flatMap/map, no printlns, and no comments
	val selectDataFromStuff: Transformer[XMLEvent, Data] = for {
		infoId <- stuffInfoParser.followedByStream
		_ <- XMLSplitter("stuff" \ "thing")
		contextId <- thingContextParser.followedByStream
		value <- thingDataTransform
	} yield Data(infoId, contextId, value)

	/** Stream side effect that will print a message for each event.
	  * Put this in front of the `selectData` transformer to see when
	  * the selectData transformer finds its events.
 	  */
	val printEvent = Transformer.sideEffect[XMLEvent](e => println(s"event: [${e.toString.trim}]"))

	/** Consumer for `Data` objects that prints a message for each one.
	  * This consumer's result type is Unit because all it does are side effects.
	  */
	val printData = Parser.foreach[Data]{ d => println(s"RESULT - $d") }

	// Data(DYLAN, A, 123)
	// Data(DYLAN, A, 124)
	// ...
	// Data(DYLAN, B, 568
	// Data(DYLAN, B, 569)
	/*printEvent >>*/ verboseSelectDataFromStuff >> printData parse xml
}
