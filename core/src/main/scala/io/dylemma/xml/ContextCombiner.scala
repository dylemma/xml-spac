package io.dylemma.xml

/** Typeclass to witness that a type `T` is *not* a Chain */
final class NonChain[-T] private()
object NonChain {
	implicit def provideAnyNonChain[T] = new NonChain[T]
	implicit def ambiguousChainNonChain[C <: Chain[_, _]] = new NonChain[C]
}
/** Typeclass to witness that a type `T` is *not* a Unit */
final class NonUnit[-T] private()
object NonUnit {
	implicit def provideAnyNonUnit[T] = new NonUnit[T]
	implicit val ambiguousUnitNonUnit1 = new NonUnit[Unit]
	implicit val ambiguousUnitNonUnit2 = new NonUnit[Unit]
}

trait ContextCombiner[-A, -B, +AB]{
	def combine(left: A, right: B): AB
}
object ContextCombiner {
	import ChainSyntax._

	private def makeCombiner[A, B, AB](f: (A, B) => AB): ContextCombiner[A, B, AB] = new ContextCombiner[A, B, AB] {
		def combine(left: A, right: B) = f(left, right)
	}

	/* Note the use of the NonUnit and NonChain typeclasses here.
	 * This is to disambiguate the generic cases like Any+Any where
	 * there is a more specialized case like Chain+Chain.
	 */

	// Unit + T = T
	implicit def provideUnitAnyCombiner[T]: ContextCombiner[Unit, T, T] = makeCombiner{ (u, t) => t }

	// T + Unit = T
	implicit def provideAnyUnitCombiner[T: NonUnit]: ContextCombiner[T, Unit, T] = makeCombiner{ (t, u) => t }

	// A + B = A ~ B
	implicit def provideAnyAnyCombiner[A: NonUnit: NonChain, B: NonUnit: NonChain]
	: ContextCombiner[A, B, A ~ B] = makeCombiner{ _ ~ _ }

	// A + C:Chain = A ~: C
	implicit def provideAnyChainCombiner[A: NonUnit: NonChain, C <: Chain[_,_], R <: Chain[_, _]](
		implicit prepender: ChainPrepend[A, C, R]): ContextCombiner[A, C, R] = makeCombiner{ _ ~: _ }

	// This case *should* be covered by the AnyAnyCombiner
	// C:Chain + A = C ~ A
	implicit def provideChainAnyCombiner[C <: Chain[_, _], A: NonUnit: NonChain]: ContextCombiner[C, A, C ~ A] = makeCombiner{ _ ~ _ }

	// A:Chain + B:Chain = A ++ B
	implicit def provideChainChainCombiner[A <: Chain[_, _], B <: Chain[_, _], AB <: Chain[_, _]](
		implicit concat: ChainConcat[A, B, AB]): ContextCombiner[A, B, AB] = makeCombiner{ _ ++ _ }
}
