package io.dylemma.xml

import javax.xml.stream.events.XMLEvent
import scala.annotation.implicitNotFound
import scala.concurrent.ExecutionContext

import io.dylemma.xml.Parser.Result
import play.api.libs.iteratee.{ Enumeratee, Iteratee }

/** A Parser Combiner is able to combine two parsers into a single parser,
  * finding the most-specific common type between the two parsers' contexts,
  * and combining their results as a chain.
  *
  * In order to be combined, the context types for both parsers must exist
  * in the same hierarchy. For example, contexts of `Any` and `String` may
  * be combined because `String` is a subtype of `Any`. But contexts of
  * `String` and `Boolean` may not be combined because neither `String` nor
  * `Boolean` are subtypes of each other.
  *
  * Most parsers will have a context type of `Any`, meaning they can be
  * trivially combined with other parsers. Combining with a parser with
  * a more specific context will narrow the applicable context of the
  * combined parser.
  *
  * Import `ParserCombinerOps._` or mixin the `ParserCombinerOps` trait to
  * add the `combine` method to parsers, which uses this type class.
  *
  * @tparam C1 The context type of the first parser
  * @tparam C2 The context type of the second parser
  * @tparam C The common context type, used by the combined parser
  */
@implicitNotFound("Parser contexts of ${C1} and ${C2} cannot be combined")
trait ParserCombiner[C1, C2, C] {
	def combine[T1, T2](parser1: Parser[C1, T1], parser2: Parser[C2, T2]): Parser[C, Chain[T1, T2]]
}
object ParserCombiner {
	private def makeCombiner[C1, C2, C](conv1: C => C1, conv2: C => C2): ParserCombiner[C1, C2, C] = new ParserCombiner[C1, C2, C] {
		override def combine[T1, T2](parser1: Parser[C1, T1], parser2: Parser[C2, T2]): Parser[C, Chain[T1, T2]] = new Parser[C, Chain[T1, T2]] {
			override def toIteratee(context: C)(implicit ec: ExecutionContext): Iteratee[XMLEvent, Result[Chain[T1, T2]]] = {
				val itr1 = parser1 toIteratee conv1(context)
				val itr2 = parser2 toIteratee conv2(context)
				Enumeratee.zipWith(itr1, itr2) { (r1, r2) =>
					for (t1 <- r1; t2 <- r2) yield Chain(t1, t2)
				}
			}
		}
	}

	/** Ensure parsers with the same context type can be combined,
	  * resulting in a parser with the same context type.
	  */
	implicit def sameContextCombiner[C]: ParserCombiner[C, C, C] = makeCombiner(identity, identity)

	/** Allow two parsers to be combined when the context of the second parser
	  * is a subtype of the context of the first parser. The resulting parser's
	  * context type will be equal to the second, as it is the most specific type.
	  */
	implicit def commonLeftCombiner[C1, C2 <: C1]: ParserCombiner[C1, C2, C2] = makeCombiner(identity, identity)

	/** Allow two parsers to be combined when the context of the first parser
	  * is a subtype of the context of the second parser. The resulting parser's
	  * context type will be equal to the first, as it is the most specific type.
	  */
	implicit def commonRightCombiner[C2, C1 <: C2]: ParserCombiner[C1, C2, C1] = makeCombiner(identity, identity)
}

object ParserCombinerOps extends ParserCombinerOps
trait ParserCombinerOps {
	implicit class ParserWithCombine[C1, T1](parser1: Parser[C1, T1]){
		/** Combine this parser with a second parser.
		  * The resulting parser's context type will be the most specific type between this
		  * parser's context and the second parser's context.
		  * The resulting parser's result type will be a chain of the two inner parser's results.
		  */
		def &[C2, T2, C](parser2: Parser[C2, T2])(implicit combiner: ParserCombiner[C1, C2, C]): Parser[C, Chain[T1, T2]] = {
			combiner.combine(parser1, parser2)
		}
	}
}