package io.dylemma.spac.syntax

import javax.xml.stream.events.XMLEvent

import io.dylemma.spac.{Consumer, Parser, Transformer}

import scala.util.Try

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
		def parseWith[B](consumer: Consumer[A, B]): Parser[B] = {
			val hf = t >> consumer
			Parser.from(hf)
		}

		def parseToList: Parser[List[A]] = parseWith(Consumer.toList)
		def parseFirst: Parser[A] = parseWith(Consumer.first)
		def parseFirstOption: Parser[Option[A]] = parseWith(Consumer.firstOption)
		def parseAsFold[R](init: R)(f: (R, A) => R): Parser[R] = parseWith(Consumer.fold(init, f))
	}

}
