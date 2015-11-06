package io.dylemma.xml

trait MatcherSemantics[Input] {

	trait Matcher[+A]{
		def apply(input: Input): Option[A]

		def &[B, AB](that: Matcher[B])(implicit rc: ResultCombiner[A, B, AB]): Matcher[AB] = Matcher.combine(this, that)
		def /[B, AB](that: Matcher[B])(implicit rc: ResultCombiner[A, B, AB]): ListMatcher[AB] = {
			ListMatcher.inductive(ListMatcher.single(this), ListMatcher.single(that))
		}

		def matchContext(context: List[Input]): Option[A] = context match {
			case Nil => None
			case head :: tail => apply(head)
		}
	}

	trait ListMatcher[+A]{
		def apply(inputs: List[Input]): Option[(A, List[Input])]
		def /[B, AB](next: Matcher[B])(implicit rc: ResultCombiner[A, B, AB]): ListMatcher[AB] = {
			ListMatcher.inductive(this, ListMatcher.single(next))
		}

		def matchContext(context: List[Input]): Option[A] = apply(context).map(_._1)
	}

	object Matcher {
		def apply[A](f: Input => Option[A]): Matcher[A] = new Matcher[A] {
			def apply(elem: Input) = f(elem)
		}
		def predicate(f: Input => Boolean): Matcher[Any] = new Matcher[Unit] {
			def apply(elem: Input) = if (f(elem)) Some(()) else None
		}
		def combine[A, B, AB](left: Matcher[A], right: Matcher[B])(implicit rc: ResultCombiner[A, B, AB]): Matcher[AB] = {
			new Matcher[AB] {
				def apply(input: Input) = for(a <- left(input); b <- right(input)) yield rc.combine(a, b)
			}
		}
	}

	object ListMatcher {
		def single[A](matcher: Matcher[A]): ListMatcher[A] = new ListMatcher[A] {
			def apply(inputs: List[Input]) = inputs match {
				case Nil => None
				case head :: tail => matcher(head).map(_ -> tail)
			}
		}

		def inductive[A, B, AB](leading: ListMatcher[A], next: ListMatcher[B])(
			implicit rc: ResultCombiner[A, B, AB]): ListMatcher[AB] = new ListMatcher[AB] {
			def apply(inputs: List[Input]) = for {
				(leadMatch, leadTail) <- leading(inputs)
				(nextMatch, nextTail) <- next(leadTail)
			} yield rc.combine(leadMatch, nextMatch) -> nextTail
		}
	}

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

	trait ResultCombiner[-A, -B, +AB]{
		def combine(left: A, right: B): AB
	}
	object ResultCombiner {
		import ChainSyntax._

		private def makeCombiner[A, B, AB](f: (A, B) => AB): ResultCombiner[A, B, AB] = new ResultCombiner[A, B, AB] {
			def combine(left: A, right: B) = f(left, right)
		}

		/* Note the use of the NonUnit and NonChain typeclasses here.
		 * This is to disambiguate the generic cases like Any+Any where
		 * there is a more specialized case like Chain+Chain.
		 */

		// Unit + T = T
		implicit def provideUnitAnyCombiner[T]: ResultCombiner[Unit, T, T] = makeCombiner{ (u, t) => t }

		// T + Unit = T
		implicit def provideAnyUnitCombiner[T: NonUnit]: ResultCombiner[T, Unit, T] = makeCombiner{ (t, u) => t }

		// A + B = A ~ B
		implicit def provideAnyAnyCombiner[A: NonUnit: NonChain, B: NonUnit: NonChain]
		: ResultCombiner[A, B, A ~ B] = makeCombiner{ _ ~ _ }

		// A + C:Chain = A ~: C
		implicit def provideAnyChainCombiner[A: NonUnit: NonChain, C <: Chain[_,_], R <: Chain[_, _]](
			implicit prepender: ChainPrepend[A, C, R]): ResultCombiner[A, C, R] = makeCombiner{ _ ~: _ }

		// This case *should* be covered by the AnyAnyCombiner
		// C:Chain + A = C ~ A
		implicit def provideChainAnyCombiner[C <: Chain[_, _], A: NonUnit: NonChain]: ResultCombiner[C, A, C ~ A] = makeCombiner{ _ ~ _ }

		// A:Chain + B:Chain = A ++ B
		implicit def provideChainChainCombiner[A <: Chain[_, _], B <: Chain[_, _], AB <: Chain[_, _]](
			implicit concat: ChainConcat[A, B, AB]): ResultCombiner[A, B, AB] = makeCombiner{ _ ++ _ }
	}

}