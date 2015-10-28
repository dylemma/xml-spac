package io.dylemma.xml

/**
 * Created by dylan on 10/27/2015.
 */
case class Chain[+H, +T](head: H, tail: T){
	def ~[U](newTail: U) = Chain(this, newTail)

	override def toString = s"$head ~ $tail"
}

object ChainSyntax {

	/** Typeclass that knows how to concatenate a Prefix chain
		* with a Suffix chain to create a Result chain
		* @tparam Prefix
		* @tparam Suffix
		* @tparam Result
		*/
	trait ChainConcat[
		Prefix <: Chain[_, _],
		Suffix <: Chain[_, _],
		Result <: Chain[_, _]
	]{
		def concat(prefix: Prefix, suffix: Suffix): Result
	}

	/** Introduces the `concat` and `++` methods on a chain when there
		* is an appropriate `ChainConcat` instance available.
		* @param prefix
		* @tparam P
		*/
	implicit class ChainConcatOps[P <: Chain[_, _]](prefix: P){
		// e.g. (A ~ B ~ C) concat (D ~ E) = (A ~ B ~ C ~ D ~ E)
		def concat[S <: Chain[_, _], Result <: Chain[_, _]](suffix: S)(implicit chainConcat: ChainConcat[P, S, Result]): Result = {
			chainConcat.concat(prefix, suffix)
		}
		// operator equivalent of `concat`
		def ++[S <: Chain[_, _], Result <: Chain[_, _]](suffix: S)(implicit chainConcat: ChainConcat[P, S, Result]): Result = {
			concat(suffix)
		}
	}

	implicit class anyChainAssoc[A](head: A) {
		def ~[B](tail: B) = Chain(head, tail)
	}

	/** Typeclass that witnesses that a type `T` is not a `Chain`. */
	sealed trait IsNotAChain[T]
	final class AnyIsNotAChain[T] extends IsNotAChain[T]
	final class ChainIsNotAChainForAmbiguity[T <: Chain[_, _]] extends IsNotAChain[T]
	implicit def anythingIsNotAChain[T]: IsNotAChain[T] = new AnyIsNotAChain[T]
	implicit def chainIsNotAChainFormAmbiguity[T <: Chain[_, _]]: IsNotAChain[T] = ??? //new ChainIsNotAChainForAmbiguity[T]
	implicit def chainIsNotAChainForAmbiguity2[T <: Chain[_, _]]: IsNotAChain[T] = ??? //new ChainIsNotAChainForAmbiguity[T]

	/** ChainConcat implementation that handles concatenation of a two-item chain
		* by appending the head item, then appending the tail item.
		*
		* P ++ (A ~ B) = P ~ A ~ B
	 */
	class SimpleChainConcat[P <: Chain[_, _], SH, ST](shNotChain: IsNotAChain[SH], stNotAChain: IsNotAChain[ST])
		extends ChainConcat[P, Chain[SH, ST], Chain[Chain[P, SH], ST]] {

		def concat(prefix: P, suffix: Chain[SH, ST]) = {
			Chain(Chain(prefix, suffix.head), suffix.tail)
		}
	}
	implicit def provideSimpleChainConcat[P <: Chain[_, _], SH: IsNotAChain, ST: IsNotAChain]
		: ChainConcat[P, Chain[SH, ST], Chain[Chain[P, SH], ST]] = {
		new SimpleChainConcat(implicitly, implicitly)
	}

	/** ChainConcat implementation that handles concatenation of a longer chain
		* by finding another ChainConcat that can handle all but the last element
		* in that chain, then simply appending the last element to that result.
		*
		* P ++ (Chain ~ ST) = (P ++ Chain) ~ ST
	 */
	class InductiveChainConcat[P <: Chain[_, _], SH <: Chain[_, _], ST, R <: Chain[_, _]](
		pstConcat: ChainConcat[P, SH, R]
	) extends ChainConcat[P, Chain[SH, ST], Chain[R, ST]] {
		def concat(prefix: P, suffix: Chain[SH, ST]) = {
			Chain(pstConcat.concat(prefix, suffix.head), suffix.tail)
		}
	}
	implicit def provideInductiveChainConcat[P <: Chain[_, _], SH <: Chain[_, _], ST, R <: Chain[_, _]]
		(implicit pstConcat: ChainConcat[P, SH, R]): ChainConcat[P, Chain[SH, ST], Chain[R, ST]] = {
		new InductiveChainConcat(pstConcat)
	}

}

object ChainSyntaxTesting extends App {
	import ChainSyntax._
	val ~ = Chain

	val a = 1 ~ "hello" ~ true ~ List(1,2,3) ~ Option(3) ~ Map(1->'c', 2->'d')
	val b = 5.234 ~ false
	println("\na ++ b\n======")
	val ab = a concat b
	println(s"result: $ab")

	println("\nb ++ a\n======")
	val ba = b concat a
	println(s"result: $ba")

	ab match {
		case i ~ s ~ b ~ list ~ opt ~ map ~ d ~ t =>
	}

}
