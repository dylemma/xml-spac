package io.dylemma.spac

import scala.collection.IndexedSeq

/** An object responsible for inspecting a stack of `StartElement` events and determining if they correspond
  * to some "context" value of type `A`.
  *
  * `ContextMatcher`s play a primary role in splitting an XML event stream into "substreams", i.e. each
  * substream is defined as the series of consecutive events during which the XML tag stack matches a context.
  *
  * `ContextMatcher`s are intended to be transformed and combined with each other in order to build up
  * more complex matching functionality. See also: `SingleElementContextMatcher`, which contains additional
  * combination methods and some specialized transformation methods.
  *
  * @tparam A The type of the matched context.
  * @group context
  */
trait ContextMatcher[Elem, +A] {

	/** The underlying context match method.
	  *
	  * Inspects the elements in the XML "tag stack", which is essentially a `List[StartElement]`, but for
	  * performance reasons is represented as an array with an "offset" index and a number of available
	  * elements from that offset. If the elements correspond to a context value of `A`, the implementation
	  * must then pass the remaining elements of the stack to the `next` matcher, i.e. by calling
	  * `next(stack, offset + numConsumed, avail - numConsumed)`.
	  *
	  * The `next` matcher is necessary in order to support non-greedy matchers, e.g. `ContextMatcher.variableLength`,
	  * a.k.a. `**`. Without a reference to the `next` matcher in the chain, matcher implementations would be
	  * forced to pick a fixed number of elements for matching, never knowing that the overall match could have
	  * succeeded if they had consumed some additional elements.
	  *
	  * @param stack  A reference to the complete XML "tag stack". Note that the responsibility of this method
	  *               is limited to a *slice* of this value, as defined by `offset` and `avail`.
	  * @param offset The index of the first element to be considered by the matching logic. From this method's
	  *               point of view, the "first" element in the stack is actually at `stack(offset)`
	  * @param avail  The number of elements available in the `stack` starting from the `offset`.
	  * @param next   The next matcher in the chain.
	  * @tparam B The `next` matcher's context type
	  * @return If the match succeeded, and the `next` match succeded, an Option containing a tuple of both match results.
	  *         If the match failed, or if the `next` match failed, `None`.
	  */
	def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[Elem, B]): Option[(A, B)]

	/** The main context match method.
	  *
	  * Inspects the elements in the XML "tag stack", which is essentially a `List[StartElement]`, but for
	  * performance reasons is represented as an array with an "offset" index and a number of available
	  * elements from that offset. If the elements correspond to a context value of `A`, the implementation
	  * must then pass the remaining elements of the stack to the `next` matcher, i.e. by calling
	  * `next(stack, offset + numConsumed, avail - numConsumed)`.
	  *
	  * The difference between this method and `applyChained` is the lack of the `next` parameter; in this
	  * method, the current matcher is assumed to be the end of the chain.
	  *
	  * @param stack  A reference to the complete XML "tag stack". Note that the responsibility of this method
	  *               is limited to a *slice* of this value, as defined by `offset` and `avail`.
	  * @param offset The index of the first element to be considered by the matching logic. From this method's
	  *               point of view, the "first" element in the stack is actually at `stack(offset)`
	  * @param avail  The number of elements available in the `stack` starting from the `offset`.
	  * @return An option containing the successfully-matched context, or `None`.
	  */
	def apply(stack: IndexedSeq[Elem], offset: Int, avail: Int): Option[A] = applyChained(stack, offset, avail, ContextMatcher.noopSuccess) map {_._1}

	/** Create a new matcher by forming a chain with this matcher at the front, and the `next` matcher at the back.
	  * In other words, a matcher for a context within another context.
	  *
	  * @param next   A matcher which will be used to match the "inner" context
	  * @param reduce The `TypeReduce` rule to help omit `Unit` from the resulting context type
	  * @tparam A1 To satisfy covariance on A
	  * @tparam B  The `next` matcher's context type
	  * @tparam R  The "reduced" content type, derived from the tuple type `(A, B)` based on the `reduce` rule.
	  * @return A matcher which delegates to `this` matcher first, then the `next` matcher for the remaining stack.
	  */
	def \[A1 >: A, B, R](next: ContextMatcher[Elem, B])(implicit reduce: TypeReduce.Aux[A1, B, R]): ContextMatcher[Elem, R] = ContextMatcher.Chained(this, next)

	/** Create a new ContextMatcher which takes the match result of this matcher and passes it through the
	  * transformation function `f`.
	  *
	  * @param f The transformation function
	  * @tparam B The transformed context type
	  * @return A new matcher with transformed results
	  */
	def map[B](f: A => B): ContextMatcher[Elem, B] = ContextMatcher.Mapped(this, "map") { a => Some(f(a)) }

	/** Create a new ContextMatcher which takes the match result of this matcher and passes it through the
	  * combined transformation/validation function `f`. If `f` returns `None`, the match is unsuccessful;
	  * if `f` returns a `Some`, the value inside is the result of the match.
	  *
	  * @param f The transformation/validation function
	  * @tparam B The transformed context type
	  * @return A new matcher with transformed and validated results
	  */
	def flatMap[B](f: A => Option[B]): ContextMatcher[Elem, B] = ContextMatcher.Mapped(this, "flatMap")(f)

	/** Create a new ContextMatcher which takes the match result of this matcher and passes it through the
	  * validation function `f`. If `f` returns `false`, the match is unsuccessful.
	  *
	  * @param p The filter predicate, i.e. the validation function
	  * @return A new matcher with validated results
	  */
	def filter(p: A => Boolean): ContextMatcher[Elem, A] = ContextMatcher.Mapped(this, "filter") { a => if (p(a)) Some(a) else None }

	/** Create a new ContextMatcher which will fall back to a second matcher in the event that this
	  * matcher fails to match a context.
	  *
	  * @param that The matcher which will be used as the fallback
	  * @tparam A2 The resulting context type (common supertype between this matcher and `that`)
	  * @return A matcher that falls back to another matcher in case of failure
	  */
	def or[A2 >: A](that: ContextMatcher[Elem, A2]): ContextMatcher[Elem, A2] = ContextMatcher.Or(this, that)

	/** Operator version of `or` */
	def |[A2 >: A](that: ContextMatcher[Elem, A2]): ContextMatcher[Elem, A2] = or(that)
}

/**
  * @group context
  */
object ContextMatcher {
	/** A matcher that quickly matches any input as `()` without consuming any stack. */
	def noopSuccess[Elem]: ContextMatcher[Elem, Unit] = new ContextMatcher[Elem, Unit] {
		override def toString = "noopSuccess"
		private val result = Some(())
		override def apply(stack: IndexedSeq[Elem], offset: Int, avail: Int) = result
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[Elem, B]): Option[(Unit, B)] = next(stack, offset, avail) map {() -> _}
	}

	/** A matcher that quickly rejects any input */
	def noopFailure[Elem]: ContextMatcher[Elem, Unit] = new ContextMatcher[Elem, Unit] {
		override def toString = "noopFailure"
		override def apply(stack: IndexedSeq[Elem], offset: Int, avail: Int): Option[Unit] = None
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[Elem, B]): Option[(Unit, B)] = None
	}

	/** A matcher that matches any input as long as the next matcher in
	  * the chain will match some segment of that input.
	  */
	def variableLength[Elem]: ContextMatcher[Elem, Unit] = new ContextMatcher[Elem, Unit] {
		override def toString = "**"
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[Elem, B]): Option[(Unit, B)] = {
			next(stack, offset, avail) map {() -> _} orElse {
				if (avail <= 0) None
				else applyChained(stack, offset + 1, avail - 1, next)
			}
		}
	}

	/** A matcher that uses a custom function `f` to consume the first `N` elements
	  * from the input before passing the remainder to the next matcher in the chain.
	  *
	  * For example:
	  * {{{
	  *   Matcher.greedy { (stack, offset, avail) =>
	  *     if(avail >= 2 && stack(offset) == "foo" && stack(offset + 1) == "bar"){
	  *       Some("yes!" -> 2)
	  *     } else {
	  *       None
	  *     }
	  *   }
	  * }}}
	  *
	  * @param f The matching function. Given a reference to the stack elements, an offset position, and
	  *          the number of elements available starting from the offset, `f` should return
	  *          an option containing a pair of the matched value and the unconsumed portion of the stack.
	  * @tparam A The matcher function's result type
	  * @return A new matcher which uses `f` to determine how (and how much of) the stack is matched
	  */
	def greedy[Elem, A](f: (IndexedSeq[Elem], Int, Int) => Option[(A, Int)]): ContextMatcher[Elem, A] = new ContextMatcher[Elem, A] {
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[Elem, B]): Option[(A, B)] = {
			for {
				(a, numConsumed) <- f(stack, offset, avail)
				b <- next(stack, offset + numConsumed, avail - numConsumed)
			} yield a -> b
		}
	}

	/** Matcher implementation for `headM \ tailM`
	  *
	  * @param headM  The first matcher in the chain
	  * @param tailM  The next matcher in the chain
	  * @param reduce The `TypeReduce` rule to combine the head and tail result types
	  * @tparam H The head result type
	  * @tparam T The tail result type
	  * @tparam F The combined result type
	  */
	case class Chained[Elem, H, T, F](headM: ContextMatcher[Elem, H], tailM: ContextMatcher[Elem, T])(implicit reduce: TypeReduce.Aux[H, T, F]) extends ContextMatcher[Elem, F] {
		override def toString = s"$headM \\ $tailM"
		override def apply(stack: IndexedSeq[Elem], offset: Int, avail: Int): Option[F] = {
			headM.applyChained(stack, offset, avail, tailM) map { (lr) => reduce.apply(lr._1, lr._2) }
		}
		def applyChained[N](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[Elem, N]): Option[(F, N)] = {
			headM.applyChained(stack, offset, avail, tailM \ next) map { case (h, (t, n)) => (reduce(h, t), n) }
		}
	}

	/** Matcher implementation for the `map`, `flatMap`, and `filter` operations.
	  *
	  * @param inner The transformed mapper
	  * @param op    The name of the operation, used by `toString`
	  * @param f     The transform operation
	  * @tparam A The type of the matched context.
	  * @tparam B The transformed context type
	  */
	case class Mapped[Elem, A, B](inner: ContextMatcher[Elem, A], op: String = "map")(f: A => Option[B]) extends ContextMatcher[Elem, B] {
		override def toString = s"$inner.$op($f)"
		def applyChained[T](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[Elem, T]): Option[(B, T)] = {
			for {
				(a, t) <- inner.applyChained(stack, offset, avail, next)
				b <- f(a)
			} yield b -> t
		}
		override def map[B2](g: B => B2): ContextMatcher[Elem, B2] = Mapped(inner, s"$op+map") { a => f(a).map(g) }
		override def flatMap[B2](g: B => Option[B2]): ContextMatcher[Elem, B2] = Mapped(inner, s"$op+flatMap") { a => f(a).flatMap(g) }
		override def filter(p: B => Boolean): ContextMatcher[Elem, B] = Mapped(inner, s"$op+filter") { a => f(a).filter(p) }
	}

	/** Matcher implementation for `left | right`.
	  *
	  * @param left  The left matcher (i.e. the first choice)
	  * @param right The right matcher (i.e. the fallback)
	  * @tparam A The type of the matched context.
	  */
	case class Or[Elem, A](left: ContextMatcher[Elem, A], right: ContextMatcher[Elem, A]) extends ContextMatcher[Elem, A] {
		override def toString = s"($left | $right)"
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[Elem, B]): Option[(A, B)] = {
			left.applyChained(stack, offset, avail, next) orElse right.applyChained(stack, offset, avail, next)
		}
	}
}


