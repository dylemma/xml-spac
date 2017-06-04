package io.dylemma.spac

import javax.xml.stream.events.{StartElement => Elem}

import io.dylemma.spac.types.TypeReduce

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
  */
trait ContextMatcher[+A] {

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
	def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[B]): Option[(A, B)]

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
	  * @tparam B The `next` matcher's context type
	  * @tparam R The "reduced" content type, derived from the tuple type `(A, B)` based on the `reduce` rule.
	  * @return A matcher which delegates to `this` matcher first, then the `next` matcher for the remaining stack.
	  */
	def \[B, R](next: ContextMatcher[B])(implicit reduce: TypeReduce.Aux[(A, B), R]): ContextMatcher[R] = ContextMatcher.Chained(this, next)

	/** Create a new ContextMatcher which takes the match result of this matcher and passes it through the
	  * transformation function `f`.
	  *
	  * @param f The transformation function
	  * @tparam B The transformed context type
	  * @return A new matcher with transformed results
	  */
	def map[B](f: A => B): ContextMatcher[B] = ContextMatcher.Mapped(this, "map") { a => Some(f(a)) }

	/** Create a new ContextMatcher which takes the match result of this matcher and passes it through the
	  * combined transformation/validation function `f`. If `f` returns `None`, the match is unsuccessful;
	  * if `f` returns a `Some`, the value inside is the result of the match.
	  *
	  * @param f The transformation/validation function
	  * @tparam B The transformed context type
	  * @return A new matcher with transformed and validated results
	  */
	def flatMap[B](f: A => Option[B]): ContextMatcher[B] = ContextMatcher.Mapped(this, "flatMap")(f)

	/** Create a new ContextMatcher which takes the match result of this matcher and passes it through the
	  * validation function `f`. If `f` returns `false`, the match is unsuccessful.
	  *
	  * @param p The filter predicate, i.e. the validation function
	  * @return A new matcher with validated results
	  */
	def filter(p: A => Boolean): ContextMatcher[A] = ContextMatcher.Mapped(this, "filter") { a => if (p(a)) Some(a) else None }

	/** Create a new ContextMatcher which will fall back to a second matcher in the event that this
	  * matcher fails to match a context.
	  *
	  * @param that The matcher which will be used as the fallback
	  * @tparam A2 The resulting context type (common supertype between this matcher and `that`)
	  * @return A matcher that falls back to another matcher in case of failure
	  */
	def or[A2 >: A](that: ContextMatcher[A2]): ContextMatcher[A2] = ContextMatcher.Or(this, that)

	/** Operator version of `or` */
	def |[A2 >: A](that: ContextMatcher[A2]): ContextMatcher[A2] = or(that)
}

object ContextMatcher {
	/** A matcher that quickly matches any input as `()` without consuming any stack. */
	val noopSuccess: ContextMatcher[Unit] = new ContextMatcher[Unit] {
		override def toString = "noopSuccess"
		private val result = Some(())
		override def apply(stack: IndexedSeq[Elem], offset: Int, avail: Int) = result
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[B]): Option[(Unit, B)] = next(stack, offset, avail) map {() -> _}
	}

	/** A matcher that quickly rejects any input */
	val noopFailure: ContextMatcher[Unit] = new ContextMatcher[Unit] {
		override def toString = "noopFailure"
		override def apply(stack: IndexedSeq[Elem], offset: Int, avail: Int): Option[Unit] = None
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[B]): Option[(Unit, B)] = None
	}

	/** A matcher that matches any input as long as the next matcher in
	  * the chain will match some segment of that input.
	  */
	val variableLength: ContextMatcher[Unit] = new ContextMatcher[Unit] {
		override def toString = "**"
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[B]): Option[(Unit, B)] = {
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
	def greedy[A](f: (IndexedSeq[Elem], Int, Int) => Option[(A, Int)]): ContextMatcher[A] = new ContextMatcher[A] {
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[B]): Option[(A, B)] = {
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
	case class Chained[H, T, F](headM: ContextMatcher[H], tailM: ContextMatcher[T])(implicit reduce: TypeReduce.Aux[(H, T), F]) extends ContextMatcher[F] {
		override def toString = s"$headM \\ $tailM"
		override def apply(stack: IndexedSeq[Elem], offset: Int, avail: Int): Option[F] = {
			headM.applyChained(stack, offset, avail, tailM) map {reduce.apply}
		}
		def applyChained[N](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[N]): Option[(F, N)] = {
			headM.applyChained(stack, offset, avail, tailM \ next) map { case (h, (t, n)) => (reduce(h -> t), n) }
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
	case class Mapped[A, B](inner: ContextMatcher[A], op: String = "map")(f: A => Option[B]) extends ContextMatcher[B] {
		override def toString = s"$inner.$op($f)"
		def applyChained[T](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[T]): Option[(B, T)] = {
			for {
				(a, t) <- inner.applyChained(stack, offset, avail, next)
				b <- f(a)
			} yield b -> t
		}
		override def map[B2](g: B => B2): ContextMatcher[B2] = Mapped(inner, s"$op+map") { a => f(a).map(g) }
		override def flatMap[B2](g: B => Option[B2]): ContextMatcher[B2] = Mapped(inner, s"$op+flatMap") { a => f(a).flatMap(g) }
		override def filter(p: B => Boolean): ContextMatcher[B] = Mapped(inner, s"$op+filter") { a => f(a).filter(p) }
	}

	/** Matcher implementation for `left | right`.
	  *
	  * @param left  The left matcher (i.e. the first choice)
	  * @param right The right matcher (i.e. the fallback)
	  * @tparam A The type of the matched context.
	  */
	case class Or[A](left: ContextMatcher[A], right: ContextMatcher[A]) extends ContextMatcher[A] {
		override def toString = s"($left | $right)"
		def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[B]): Option[(A, B)] = {
			left.applyChained(stack, offset, avail, next) orElse right.applyChained(stack, offset, avail, next)
		}
	}
}

/** Specialization of ContextMatcher which only checks the first element in the stack for matching operations.
  * Transformation operations on single-element matchers will yield other single-element matchers (rather than
  * the base ContextMatcher type). Combination operations involving other single-element matchers will also
  * yield single-element matchers.
  * SingleElementContextMatchers form the building blocks of more complex matchers.
  *
  * @tparam A The type of the matched context.
  */
trait SingleElementContextMatcher[+A] extends ContextMatcher[A] {
	/** The matching operation for single-element matchers.
	  *
	  * @param elem The first element in the XML tag stack
	  * @return `Some(context)` for a successful match, `None` otherwise
	  */
	def applyElem(elem: Elem): Option[A]

	def applyChained[B](stack: IndexedSeq[Elem], offset: Int, avail: Int, next: ContextMatcher[B]): Option[(A, B)] = {
		if (avail >= 1) {
			for {
				a <- applyElem(stack(offset))
				b <- next(stack, offset + 1, avail - 1)
			} yield a -> b
		} else {
			None
		}
	}
	override def map[B](f: A => B): SingleElementContextMatcher[B] = SingleElementContextMatcher.Mapped(this, "map") { a => Some(f(a)) }
	override def flatMap[B](f: A => Option[B]): SingleElementContextMatcher[B] = SingleElementContextMatcher.Mapped(this, "flatMap")(f)
	override def filter(p: A => Boolean): SingleElementContextMatcher[A] = SingleElementContextMatcher.Mapped(this, "filter") { a => if (p(a)) Some(a) else None }

	/** Specialization of the default `or` method, specifically for SingleElementContextMatchers */
	def or[A2 >: A](that: SingleElementContextMatcher[A2]): SingleElementContextMatcher[A2] = SingleElementContextMatcher.Or(this, that)
	/** Operator version of `or`, specialized for SingleElementContextMatchers */
	def |[A2 >: A](that: SingleElementContextMatcher[A2]): SingleElementContextMatcher[A2] = SingleElementContextMatcher.Or(this, that)

	/** Creates a new single-element matcher which combines the results of both `this` matcher and `that` matcher.
	  * Both `this` and `that` will operate on the first element of the stack (as opposed to Chained matchers).
	  *
	  * @param that   The matcher to combine
	  * @param reduce The `TypeReduce` rule for combining the two match results
	  * @tparam B The other matcher's result type
	  * @tparam R The combined result type
	  * @return A new matcher which combines the results of `this` and `that`
	  */
	def and[B, R](that: SingleElementContextMatcher[B])(implicit reduce: TypeReduce.Aux[(A, B), R]): SingleElementContextMatcher[R] = {
		SingleElementContextMatcher.And(this, that)
	}
	/** Operator version of `and` */
	def &[B, R](that: SingleElementContextMatcher[B])(implicit reduce: TypeReduce.Aux[(A, B), R]): SingleElementContextMatcher[R] = {
		SingleElementContextMatcher.And(this, that)
	}
}

object SingleElementContextMatcher {

	/** Create a new single-element matcher which calls the given matcher function `f` on the first
	  * element of the stack. If `f` returns `None`, the match fails, otherwise, the match succeeds.
	  *
	  * @param f The matcher function
	  * @tparam A The match result type
	  * @return A new single-element matcher which applies `f` to the stack head
	  */
	def apply[A](f: Elem => Option[A]): SingleElementContextMatcher[A] = new Default(f)

	/** Create a new single-element matcher which calls the given matcher function `f` on the first
	  * element of the stack. If `f` returns `None`, the match fails, otherwise, the match succeeds.
	  *
	  * @param name This value will be used as the matcher's `toString`
	  * @param f    The matcher function
	  * @tparam A The match result type
	  * @return A new single-element matcher which applies `f` to the stack head
	  */
	def apply[A](name: String, f: Elem => Option[A]): SingleElementContextMatcher[A] = new Default(f) {
		override def toString = name
	}

	/** Create a new single-element matcher which calls the given predicate function `f` on the first
	  * element of the stack. If `f` returns `true`, the match succeeds. Otherwise, the match fails.
	  *
	  * @param f The predicate function
	  * @return A new single-element matcher which uses `f` to determine a match
	  */
	def predicate(f: Elem => Boolean): SingleElementContextMatcher[Unit] = new Predicate(f)

	/** Create a new single-element matcher which calls the given predicate function `f` on the first
	  * element of the stack. If `f` returns `true`, the match succeeds. Otherwise, the match fails.
	  *
	  * @param name This value will be used as the matcher's `toString`
	  * @param f    The predicate function
	  * @return A new single-element matcher which uses `f` to determine a match
	  */
	def predicate(name: String, f: Elem => Boolean): SingleElementContextMatcher[Unit] = new Predicate(f) {
		override def toString = name
	}

	/** SingleElementContextMatcher that delegates to the given function `f` to perform its match.
	  *
	  * @param f A function that performs the context match on a single element
	  * @tparam A The matched context type
	  */
	class Default[A](f: Elem => Option[A]) extends SingleElementContextMatcher[A] {
		def applyElem(elem: Elem): Option[A] = f(elem)
	}

	/** SingleElementContextMatcher that successfully matches (with no result) if the
	  * given predicate function returns `true` for the matched element.
	  *
	  * @param f The predicate function
	  */
	class Predicate(f: Elem => Boolean) extends SingleElementContextMatcher[Unit] {
		def applyElem(elem: Elem): Option[Unit] = if (f(elem)) predicateSuccess else None
	}
	private val predicateSuccess = Some(())

	/** Combines two `SingleElementContextMatcher`s such that the resulting matcher will succeed if both the
	  * `right` and `left` matchers succeed, and returns the results of both matchers (as a tuple, but reduced
	  * via the `TypeReduce` rules)
	  *
	  * @param left   The left matcher, with a context type of `A`
	  * @param right  The right matcher, with a context type of `B`
	  * @param reduce The `TypeReduce` rule
	  * @tparam A The left matcher's context type
	  * @tparam B The right matcher's context type
	  * @tparam R The type reduction of `(A, B)`
	  */
	case class And[A, B, R](left: SingleElementContextMatcher[A], right: SingleElementContextMatcher[B])(implicit reduce: TypeReduce.Aux[(A, B), R]) extends SingleElementContextMatcher[R] {
		override def toString = s"($left & $right)"
		def applyElem(elem: Elem): Option[R] = {
			for {
				a <- left.applyElem(elem)
				b <- right.applyElem(elem)
			} yield reduce(a -> b)
		}
	}

	/** Similar to `ContextMatcher.Or`, but specialized for `SingleElementContextMatcher` */
	case class Or[A](left: SingleElementContextMatcher[A], right: SingleElementContextMatcher[A]) extends SingleElementContextMatcher[A] {
		override def toString = s"($left | $right)"
		def applyElem(elem: Elem): Option[A] = left.applyElem(elem) orElse right.applyElem(elem)
	}

	/** Similar to `ContextMatcher.Mapped`, but specialized for `SingleElementContextMatcher` */
	case class Mapped[A, B](inner: SingleElementContextMatcher[A], op: String = "map")(f: A => Option[B]) extends SingleElementContextMatcher[B] {
		override def toString = s"$inner.$op($f)"
		def applyElem(elem: Elem): Option[B] = inner.applyElem(elem).flatMap(f)
		override def map[B2](g: B => B2): SingleElementContextMatcher[B2] = Mapped(inner, s"$op+map") { a => f(a).map(g) }
		override def flatMap[B2](g: B => Option[B2]): SingleElementContextMatcher[B2] = Mapped(inner, s"$op+flatMap") { a => f(a).flatMap(g) }
		override def filter(p: B => Boolean): SingleElementContextMatcher[B] = Mapped(inner, s"$op+filter") { a => f(a).filter(p) }
	}
}
