package io.dylemma.xml

import io.dylemma.xml.ChainSyntax._

sealed trait MatchResult[+T]
object MatchResult {
	case object Ok extends MatchResult[Nothing]
	case class SingleValue[T](value: T) extends MatchResult[T]
	case class ChainValues[T <: Chain[_, _]](values: T) extends MatchResult[T]
}

trait MatchResultSimplifier[M <: MatchResult[_], R]{
	def simplify(m: M): R
}
object MatchResultSimplifier {
	import MatchResult._

	// base functionality for result simplifiers
	private def simplifier[M <: MatchResult[_], R](f: M => R): MatchResultSimplifier[M, R] = {
		new MatchResultSimplifier[M, R] {
			def simplify(m: M): R = f(m)
		}
	}

	implicit val okSimplifier: MatchResultSimplifier[Ok.type, Unit] = simplifier(_ => ())
	implicit def singleValueSimplifier[T]: MatchResultSimplifier[SingleValue[T], T] = simplifier(_.value)
	implicit def chainValuesSimplifier[T <: Chain[_, _]]: MatchResultSimplifier[ChainValues[T], T] = simplifier(_.values)
}

trait MatchResultCombiner[T1 <: MatchResult[_], T2 <: MatchResult[_], R <: MatchResult[_]]{
	def combine(result1:T1, result2: T2): R
}

object MatchResultCombiner {
	import MatchResult._

	// Ok + <X> = <X>
	class OkAnyCombiner[R <: MatchResult[_]] extends MatchResultCombiner[Ok.type, R, R] {
		def combine(ok: Ok.type, result: R) = result
	}
	implicit def provideOkAnyCombiner[R <: MatchResult[_]]: OkAnyCombiner[R] = new OkAnyCombiner[R]
	// note: avoid introducing an `X + Ok` combiner as it would cause ambiguity for the `Ok + Ok` case

	// Single(s) + Ok = Single(s)
	class SingleOkCombiner[S] extends MatchResultCombiner[SingleValue[S], Ok.type, SingleValue[S]] {
		def combine(single: SingleValue[S], ok: Ok.type) = single
	}
	implicit def provideSingleOkCombiner[S]: SingleOkCombiner[S] = new SingleOkCombiner[S]

	// Single(s) + Single(t) = Chain(s ~ t)
	class SingleSingleCombiner[S, T] extends MatchResultCombiner[SingleValue[S], SingleValue[T], ChainValues[S ~ T]]{
		def combine(left: SingleValue[S], right: SingleValue[T]) = ChainValues(left.value ~ right.value)
	}
	implicit def provideSingleSingleCombiner[S, T]: SingleSingleCombiner[S, T] = new SingleSingleCombiner[S, T]

	// Single(s) + Chain(c) = Chain(s ~: c)
	class SingleChainCombiner[S, C <: Chain[_, _], R <: Chain[_, _]](implicit prepender: ChainPrepend[S, C, R])
		extends MatchResultCombiner[SingleValue[S], ChainValues[C], ChainValues[R]]{
		def combine(left: SingleValue[S], right: ChainValues[C]) = {
			ChainValues(left.value ~: right.values)
		}
	}
	implicit def provideSingleChainCombiner[S, C <: Chain[_, _], R <: Chain[_, _]](
		implicit prepender: ChainPrepend[S, C, R]
	): SingleChainCombiner[S, C, R] = new SingleChainCombiner[S, C, R]

	// Chain + Ok = Chain
	class ChainOkCombiner[C <: Chain[_, _]] extends MatchResultCombiner[ChainValues[C], Ok.type, ChainValues[C]]{
		def combine(chain: ChainValues[C], ok: Ok.type): ChainValues[C] = chain
	}
	implicit def provideChainOkCombiner[C <: Chain[_, _]]: ChainOkCombiner[C] = new ChainOkCombiner[C]

	// Chain(c) + Single(s) = Chain(c ~ s)
	class ChainSingleCombiner[C <: Chain[_, _], S] extends MatchResultCombiner[ChainValues[C], SingleValue[S], ChainValues[C ~ S]] {
		def combine(result1: ChainValues[C], result2: SingleValue[S]): ChainValues[C ~ S] = {
			ChainValues(result1.values ~ result2.value)
		}
	}
	implicit def provideChainSingleCombiner[C <: Chain[_, _], S]: ChainSingleCombiner[C, S] = new ChainSingleCombiner[C, S]

	// Chain(a) + Chain(b) = Chain(a ++ b)
	class ChainChainCombiner[A <: Chain[_, _], B <: Chain[_, _], R <: Chain[_, _]](implicit chainConcat: ChainConcat[A, B, R])
		extends MatchResultCombiner[ChainValues[A], ChainValues[B], ChainValues[R]] {
		def combine(left: ChainValues[A], right: ChainValues[B]): ChainValues[R] = {
			ChainValues(left.values ++ right.values)
		}
	}
	implicit def provideChainChainCombiner[A <: Chain[_, _], B <: Chain[_, _], R <: Chain[_, _]](
		implicit chainConcat: ChainConcat[A, B, R]
	): ChainChainCombiner[A, B, R] = new ChainChainCombiner[A, B, R]

	val okok = implicitly[MatchResultCombiner[Ok.type, Ok.type, Ok.type]]

}