package io.dylemma.spac.syntax

import io.dylemma.spac.{Consumer, FromHandlerFactory, Transformer}
import scala.language.higherKinds

trait TransformerSyntax {
	implicit class TransformerParsingOps[+In, P, Parser[+_]](t: Transformer[In, P])(implicit fhf: FromHandlerFactory[In, Parser]) {

		/** Attach a `Consumer` to this transformer to create a `Parser` instance.
		  *
		  * @param consumer A consumer that calculates a final output value based on events passed through the transformer.
		  * @param setDebugName An optional override for the generated parser's `toString`.
		  * @tparam Out The final output type
		  * @return A parser instance representing the combination of the transformer (`t`) and the `consumer`.
		  */
		def parseWith[Out](consumer: Consumer[P, Out], setDebugName: Option[String] = None): Parser[Out] = {
			val debugName = setDebugName getOrElse s"$t.parseWith($consumer)"
			fhf.makeInstance(t >> consumer, debugName)
		}

		/** Create a parser that consumes items from this transformer by building a list.
		  *
		  * @return A parser that consumes items from this transformer by building a list
		  */
		def parseToList: Parser[List[P]] = parseWith(Consumer.toList, Some(s"$t.parseToList"))

		/** Create a parser that consumes and returns the first item passed from this transformer.
		  * The parser will fail with a `NoSuchElementException` if the transformer passes an "end" before any other elements.
		  *
		  * @return A parser that returns the first item from this transformer
		  */
		def parseFirst: Parser[P] = parseWith(Consumer.first, Some(s"$t.parseFirst"))

		/** Create a parser that consumes and returns the optional first item passed from this transformer.
		  * If the transformer sends an EOF signal before any items, the returned parser will return `None`.
		  *
		  * @return A parser that returns the optional first item from this transformer, or `None`
		  */
		def parseFirstOption: Parser[Option[P]] = parseWith(Consumer.firstOption, Some(s"$t.parseFirstOption"))

		/** Create a parser that consumes items from this transformer by folding them into an accumulator function `f`.
		  * @param init The initial value for the fold. If the transformer sends an EOF, the parser result will be this value.
		  * @param f The accumulator function
		  * @tparam Out The result type
		  * @return A parser that folds over the items sent from this transformer
		  */
		def parseAsFold[Out](init: Out)(f: (Out, P) => Out): Parser[Out] = parseWith(Consumer.fold(init, f), Some(s"$t.fold($init, $f)"))
	}
}
