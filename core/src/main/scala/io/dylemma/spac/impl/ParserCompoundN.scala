package io.dylemma.spac
package impl

import cats.implicits._
import cats.{Functor, Monad}

class ParserCompoundN[F[+_] : Monad, In, Out](
	private val unfinished: List[(Int, Parser[F, In, Any])],
	private val finished: Map[Int, Any],
	private val assemble: (Int => Any) => Out
) extends Parser[F, In, Out] {

	override def toString = s"ParserCompoundN(unfinished = $unfinished, finished = $finished)"

	/** For debug/testing purposes,
	  * this combines the `unfinished` and `finished` collections
	  * in an array with each entry at its corresponding index,
	  * representing unfinished parsers as Rights and finished results as Lefts.
	  * Also, since Applicative / product builds things up from right to left,
	  * the parsers at the "end" end up having the lowest number indexes,
	  * so we reverse the output so each parser/result is located at the
	  * index corresponding to that parser's position in the source that constructed this parser.
	  */
	def inspect: IndexedSeq[Any Either Parser[F, In, Any]] = {
		val size = unfinished.size + finished.size
		val out = new Array[Any Either Parser[F, In, Any]](size)
		for ((i, p) <- unfinished) out(size - i - 1) = Right(p)
		for ((i, a) <- finished) out(size - i - 1) = Left(a)
		out.toVector
	}

	def finish: F[Out] = {
		unfinished.map { case (i, parser) =>
			parser.finish.map(i -> _)
		}.sequence.map { justFinished =>
			assemble(lazyConcatMap(finished, justFinished))
		}
	}
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = {
		unfinished.partitionEitherM { case (i, parser) =>
			parser.step(in).map {
				case Left(out) => Left(i -> out)
				case Right(cont) => Right(i -> cont)
			}
		}.map { case (addedFinished, newUnfinished) =>
			if (newUnfinished.isEmpty) {
				// all inner parsers have finished, so we can produce a result
				Left(assemble(lazyConcatMap(finished, addedFinished)))
			} else {
				// some inner parsers are still running, so continue with the remaining unfinished parsers
				Right(new ParserCompoundN(newUnfinished, lazyConcatMap(finished, addedFinished), assemble))
			}
		}
	}

	/** Optimization for `m1 ++ m2`, with the expectation that `m2` will often be empty.
	  * Since Map's `++` operator seems to be O(N+M),
	  * we avoid the cost of re-building an identical copy of either m1 or m2
	  */
	@inline def lazyConcatMap[A, B](m1: Map[A, B], m2: List[(A, B)]): Map[A, B] = {
		if (m2.isEmpty) m1
		else if (m1.isEmpty) m2.toMap
		else m1 ++ m2
	}

	/** Optimization related to Parser's `Applicative`.
	  * This function represents the `product` operation where the LHS parser is also a "compound" parser.
	  * The underlying finished/unfinished/assemble values from this parser and `fa` will be merged,
	  * returning a new "compound" parser.
	  * The general goal is to "flatten" the hierarchy created by Applicative's ap/map operations
	  * so we end up having fewer instantiations while the parser is running.
	  */
	def compoundProductWithLhs[A](fa: ParserCompoundN[F, In, A]): ParserCompoundN[F, In, (A, Out)] = {
		val offset = unfinished.size + finished.size
		val faUnfinished = fa.unfinished.map { case (i, p) => (i + offset, p) }
		val faFinished = fa.finished.map { case (i, r) => (i + offset, r) }
		val combinedAssemble = (get: Int => Any) => {
			val out = assemble(get)
			val a = fa.assemble(i => get(i + offset))
			a -> out
		}
		new ParserCompoundN(faUnfinished ++ unfinished, finished ++ faFinished, combinedAssemble)
	}

	/** Optimization related to Parser's `Applicative`.
	  * This function represents the `product` operation where the LHS parser is a non-compound parser.
	  * The LHS parser will be added as a new 'unfinished' member, to a new compound parser that will be returned.
	  * The general goal is to "flatten" the hierarchy created by Applicative's ap/map operations
	  * so we end up having fewer instantiations while the parser is running.
	  */
	def productWithLhs[A](fa: Parser[F, In, A]): ParserCompoundN[F, In, (A, Out)] = {
		val i = unfinished.size + finished.size
		new ParserCompoundN(
			(i -> fa) :: unfinished,
			finished,
			(get: Int => Any) => (get(i).asInstanceOf[A], assemble(get))
		)
	}

	/** Sort of a compromise between optimization and aesthetics, related to Parser's Applicative.
	  * If you put together something like `((p1, p2).tupled, p3).tupled`, when the `p3`
	  * gets handled by Applicative, it comes in as the `fa` for this method.
	  * Really this exists in order to satisfy the aesthetic requirement that the indexes
	  * for the underlying parsers be consistent with their order in the source code.
	  * If it weren't for that, we could just use `.productWithLhs(fa).map(_.swap).
	  */
	def productWithRhs[A](fa: Parser[F, In, A]): ParserCompoundN[F, In, (Out, A)] = {
		val _unfinished = unfinished.map { case (i, p) => (i + 1, p) }
		val _finished = finished.map { case (i, p) => (i + 1, p) }
		val _assemble = (get: Int => Any) => {
			val out = assemble(i => get(i + 1))
			val a = get(0).asInstanceOf[A]
			out -> a
		}
		new ParserCompoundN(
			(0 -> fa) :: _unfinished,
			_finished,
			_assemble
		)
	}

	/** Optimization related to Parser's `Applicative`.
	  * Since a compound parser already includes a rough analogue of the `map` function in its `assemble`,
	  * we can just wrap the `assemble` function with the mapping function `f`;
	  * the result of this wrapping persists throughout the lifetime of the returned parser.
	  * If we didn't do this, the default `.map` implementation would cause us to instantiate
	  * a new `MappedParser` at every step.
	  */
	override def map[Out2](f: Out => Out2)(implicit F: Functor[F]): Parser[F, In, Out2] = {
		new ParserCompoundN(unfinished, finished, (get: Int => Any) => f(assemble(get)))
	}
}