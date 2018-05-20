package io.dylemma.spac.xml.syntax

import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.xml.XMLParser
import io.dylemma.spac.{Consumer, Transformer}

trait TransformerSyntax {

	implicit class TransformerParsingOps[A](t: Transformer[XMLEvent, A]) {

		/** Attach a `Consumer` to this transformer to create a `Parser`.
			*
			* @param consumer All events passed through this transformer will be
			*                 passed through the given `consumer`.
			* @tparam B The type of the `Result`s emitted by the `consumer`
			* @return A `Parser` whose result is the result of the `consumer`
			*         after receiving events from this transformer.
			*/
		def parseWith[B](consumer: Consumer[A, B]): XMLParser[B] = {
			val hf = t >> consumer
			XMLParser.from(hf)
		}

		def parseToList: XMLParser[List[A]] = parseWith(Consumer.toList)
		def parseFirst: XMLParser[A] = parseWith(Consumer.first)
		def parseFirstOption: XMLParser[Option[A]] = parseWith(Consumer.firstOption)
		def parseAsFold[R](init: R)(f: (R, A) => R): XMLParser[R] = parseWith(Consumer.fold(init, f))
	}

}
