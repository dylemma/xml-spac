package xsp

case class Chain[+H, +T](head: H, tail: T){
	def ~[U](newTail: U): Chain[this.type, U] = Chain(this, newTail)

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

	val ~ = Chain
	type ~[+A, +B] = Chain[A, B]

	implicit class anyChainAssoc[A](head: A) {
		def ~[B](tail: B) = Chain(head, tail)
	}

	/** ChainConcat implementation that handles concatenation of a two-item chain
		* by appending the head item, then appending the tail item.
		*
		* P ++ (A ~ B) = P ~ A ~ B
		*/
	class SimpleChainConcat[P <: Chain[_, _], SH, ST]
		extends ChainConcat[P, Chain[SH, ST], Chain[Chain[P, SH], ST]] {

		def concat(prefix: P, suffix: Chain[SH, ST]) = {
			Chain(Chain(prefix, suffix.head), suffix.tail)
		}
	}
	implicit def provideSimpleChainConcat[P <: Chain[_, _], SH, ST]
	: ChainConcat[P, Chain[SH, ST], Chain[Chain[P, SH], ST]] = {
		new SimpleChainConcat
	}

	/** ChainConcat implementation that handles concatenation of a longer chain
		* by finding another ChainConcat that can handle all but the last element
		* in that chain, then simply appending the last element to that result.
		*
		* P ++ (Chain ~ ST) = (P ++ Chain) ~ ST
		*/
	case class InductiveChainConcat[P <: Chain[_, _], SH <: Chain[_, _], ST, R <: Chain[_, _]](
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

	/** Typeclass that knows how to prepend a single value in front of a chain.
		*
		* @tparam P The prefix value type
		* @tparam C The chain type
		* @tparam R The result chain's type
		*/
	trait ChainPrepend[P, C <: Chain[_, _], R <: Chain[_, _]]{
		def prepend(value: P, chain: C): R
	}

	/** Introduces the `prepend` and `~:` operations on chains when there
		* is an appropriate ChainPrepend instance available.
		*/
	implicit class ChainPrependOps[C <: Chain[_, _]](chain: C){
		def prepend[P, R <: Chain[_, _]](value: P)(implicit prepender: ChainPrepend[P, C, R]): R = {
			prepender.prepend(value, chain)
		}
		def ~:[P, R <: Chain[_, _]](value: P)(implicit prepender: ChainPrepend[P, C, R]): R = {
			prepend(value)
		}
	}

	/** ChainPrepend implementation that handles prepends to a two-item chain
		* by creating a three-item chain.
		*
		* P ~: (A ~ B) = P ~ A ~ B
		*/
	class SimpleChainPrepend[P, A, B] extends ChainPrepend[P, Chain[A, B], Chain[Chain[P, A], B]]{
		def prepend(value: P, chain: Chain[A, B]) = {
			Chain(Chain(value, chain.head), chain.tail)
		}
	}
	implicit def provideSimpleChainPrepend[P, A, B]: ChainPrepend[P, Chain[A, B], Chain[Chain[P, A], B]] = {
		new SimpleChainPrepend[P, A, B]
	}

	/** ChainPrepend implementation that handles prepends to longer chains by
		* delegating to another ChainPrepend that can handle prepends to the
		* chain's head (this process will recurse until the head is a two-item
		* list, at which point it will use the SimpleChainPrepend).
		*
		*  P ~: (complex ~ T) = (P ~: complex) ~ T
		*
		* @param pcPrepender
		* @tparam P The prefix value type
		* @tparam C The chain's head (an inner chain)
		* @tparam PC The result type of prepending a `P` to a `C`
		* @tparam T The chain's tail type
		*/
	class InductiveChainPrepend[P, C <: Chain[_, _], PC <: Chain[_, _], T](implicit pcPrepender: ChainPrepend[P, C, PC])
		extends ChainPrepend[P, Chain[C, T], Chain[PC, T]] {
		def prepend(value: P, chain: Chain[C, T]) = {
			Chain(pcPrepender.prepend(value, chain.head), chain.tail)
		}
	}
	implicit def provideInductiveChainPrepend[P, C <: Chain[_, _], PC <: Chain[_, _], T](
		implicit pcPrepender: ChainPrepend[P, C, PC]): ChainPrepend[P, Chain[C, T], Chain[PC, T]] = {
		new InductiveChainPrepend[P, C, PC, T]
	}
}
