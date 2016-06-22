package io.dylemma.xsp

import javax.xml.stream.events.XMLEvent

object TransformerSyntax extends TransformerSyntax
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
		def parseWith[B](consumer: Consumer[A, Result[B]]): Parser[Any, B] = {
			Parser.fromConsumer(t >> consumer)
		}

		def parseToList: Parser[Any, List[A]] = parseWith(Consumer.ToList())
		def parseFirst: Parser[Any, A] = parseWith(Consumer.First())
		def parseFirstOption: Parser[Any, Option[A]] = parseWith(Consumer.FirstOption())
		def parseAsFold[R](init: R)(f: (R, A) => R): Parser[Any, R] = parseWith(Consumer.Fold(init, f))
		def parseAsResultFold[R](init: Result[R])(
			f: (Result[R], Result[A]) => Result[R]
		): Parser[Any, R] = parseWith(Consumer.FoldResults(init, f))
	}

}
