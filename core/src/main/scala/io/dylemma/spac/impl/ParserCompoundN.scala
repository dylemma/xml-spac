package io.dylemma.spac
package impl

import cats.data.Chain

import scala.annotation.tailrec
import scala.collection.mutable

case class ParserCompoundN[In, Out](members: Chain[Parser[In, Any]], assemble: (Int => Any) => Out) extends Parser[In, Out] {
	def newHandler = {
		val pending = members.iterator.zipWithIndex.map { case (p, i) => i -> p.newHandler }.toArray
		new ParserCompoundN.Handler(pending, pending.length, mutable.Map.empty, assemble)
	}

	/** Optimization related to Parser's `Applicative`.
	  * Since a compound parser already includes a rough analogue of the `map` function in its `assemble`,
	  * we can just wrap the `assemble` function with the mapping function `f`;
	  * the result of this wrapping persists throughout the lifetime of the returned parser.
	  * If we didn't do this, the default `.map` implementation would cause us to instantiate
	  * a new `MappedParser` at every step.
	  */
	override def map[Out2](f: Out => Out2): Parser[In, Out2] = {
		ParserCompoundN(members, (get: Int => Any) => f(assemble(get)))
	}
}

object ParserCompoundN {
	class Handler[In, Out](
		pending: Array[(Int, Parser.Handler[In, Any])],
		private var numPending: Int,
		finished: mutable.Map[Int, Any],
		assemble: (Int => Any) => Out,
	) extends Parser.Handler[In, Out]
	{

		/** For debug/testing purposes,
		  * this combines the `unfinished` and `finished` collections
		  * in an array with each entry at its corresponding index,
		  * representing unfinished parsers as Rights and finished results as Lefts.
		  * Also, since Applicative / product builds things up from right to left,
		  * the parsers at the "end" end up having the lowest number indexes,
		  * so we reverse the output so each parser/result is located at the
		  * index corresponding to that parser's position in the source that constructed this parser.
		  */
		def inspect: IndexedSeq[Any Either Parser.Handler[In, Any]] = {
			val out = new Array[Any Either Parser.Handler[In, Any]](pending.length)
			for ((i, p) <- pending.iterator.take(numPending)) out(i) = Right(p)
			for ((i, a) <- finished.iterator) out(i) = Left(a)
			out.toVector
		}

		def finish() = {
			for ((j, p) <- pending.iterator.take(numPending)) finished(j) = p.finish()
			assemble(finished)
		}

		def step(in: In) = {
			@tailrec def loop(i: Int): Unit = {
				if (i < numPending) {
					val (j, p) = pending(i)
					p.step(in) match {
						case Right(cont) =>
							pending(i) = j -> cont
							loop(i + 1)
						case Left(a) =>
							finished(j) = a
							ArrayHelper.placeAndShiftLeft(pending, i, numPending - 1)
							numPending -= 1
							loop(i)
					}
				}
			}

			loop(0)
			if (numPending == 0) {
				Left(assemble(finished))
			} else {
				Right(this)
			}
		}

	}
}