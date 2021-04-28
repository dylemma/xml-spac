package io.dylemma.spac
package example

import io.dylemma.spac.xml._
import io.dylemma.spac.xml.JavaxSupport._

object Example07_Interrupter extends App {

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
	val captureOptionalContext: XmlParser[Option[String]] = Splitter.xml("stuff" \ "context").attr("id").parseFirstOpt

	// Stream of <data> given an optional greeting
	def dataStream(greeting: Option[String]) = Splitter.xml("stuff" \ "data").text.map { subject =>
		greeting.fold(s"(no greeting for) $subject"){ g => s"$g, $subject" }
	}

	val handleGreetings = captureOptionalContext.followedByStream(dataStream).parseTap(println)

	println("The greetings should be all ok here:")
	handleGreetings parse rawXml1

	println("\n\nBut we won't get *anything* here:")
	handleGreetings parse rawXml2

	// make the context capturing parser hit an early EOF when we reach a <data>
	val interrupter = Splitter.xml("stuff" \ "data").joinBy(Parser.pure("interrupt!" /* this value doesn't matter */)).parseFirst
	val betterCaptureOptionalContext = captureOptionalContext.interruptedBy(interrupter)
	val betterHandleGreetings = betterCaptureOptionalContext.followedByStream(dataStream).parseTap(println)

	println("\n\nNow the new handler:")
	betterHandleGreetings parse rawXml1

	println("\n\nAnd now with the missing context (this time it should work!):")
	betterHandleGreetings parse rawXml2
}
