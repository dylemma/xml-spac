package io.dylemma.xml.example

import io.dylemma.spac._
import io.dylemma.spac.xml._

object Example7_Interrupter extends App {

	val rawXml1 =
		"""<stuff>
		  |  <context id="Hello"/>
		  |  <data>World</data>
		  |  <data>Moon</data>
		  |  <data>There</data>
		  |</stuff>
		""".stripMargin

	val rawXml2 =
		"""<stuff>
		  |  <data>World</data>
		  |  <data>Moon</data>
		  |  <data>There</data>
		  |</stuff>
		""".stripMargin

	// capture an optional <context> element's id
	val captureOptionalContext: XMLParser[Option[String]] = XMLSplitter("stuff" \ "context").firstOption.attr("id")

	// Stream of <data> given an optional greeting
	def dataStream(greeting: Option[String]) = XMLSplitter("stuff" \ "data").asText.map { subject =>
		greeting.fold(s"(no greeting for) $subject"){ g => s"$g, $subject" }
	}

	val handleGreetings = captureOptionalContext.followedByStream(dataStream).consumeForEach(println)

	println("The greetings should be all ok here:")
	handleGreetings consume rawXml1

	println("\n\nBut we won't get *anything* here:")
	handleGreetings consume rawXml2

	// make the context capturing parser hit an early EOF when we reach a <data>
	val interrupter = XMLSplitter("stuff" \ "data").first(XMLParser.constant("interrupt!" /* this value doesn't matter */))
	val betterCaptureOptionalContext = captureOptionalContext.interruptedBy(interrupter)
	val betterHandleGreetings = betterCaptureOptionalContext.followedByStream(dataStream).consumeForEach(println)

	println("\n\nNow the new handler:")
	betterHandleGreetings consume rawXml1

	println("\n\nAnd now with the missing context (this time it should work!):")
	betterHandleGreetings consume rawXml2
}
