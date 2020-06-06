package io.dylemma.spac

import io.dylemma.spac.types.TypeReduce

import scala.collection.IndexedSeq

/** Specialization of ContextMatcher which only checks the first element in the stack for matching operations.
  * Transformation operations on single-element matchers will yield other single-element matchers (rather than
  * the base ContextMatcher type). Combination operations involving other single-element matchers will also
  * yield single-element matchers.
  * SingleElementContextMatchers form the building blocks of more complex matchers.
  *
  * @tparam A The type of the matched context.
  */
trait SingleItemContextMatcher[Item, +A] extends ContextMatcher[Item, A] {
	/** The matching operation for single-element matchers.
	  *
	  * @param elem The first element in the XML tag stack
	  * @return `Some(context)` for a successful match, `None` otherwise
	  */
	def applyElem(elem: Item): Option[A]

	def applyChained[B](stack: IndexedSeq[Item], offset: Int, avail: Int, next: ContextMatcher[Item, B]): Option[(A, B)] = {
		if (avail >= 1) {
			for {
				a <- applyElem(stack(offset))
				b <- next(stack, offset + 1, avail - 1)
			} yield a -> b
		} else {
			None
		}
	}
	override def map[B](f: A => B): SingleItemContextMatcher[Item, B] = SingleItemContextMatcher.Mapped(this, "map") { a => Some(f(a)) }
	override def flatMap[B](f: A => Option[B]): SingleItemContextMatcher[Item, B] = SingleItemContextMatcher.Mapped(this, "flatMap")(f)
	override def filter(p: A => Boolean): SingleItemContextMatcher[Item, A] = SingleItemContextMatcher.Mapped(this, "filter") { a => if (p(a)) Some(a) else None }

	/** Specialization of the default `or` method, specifically for SingleElementContextMatchers */
	def or[A2 >: A](that: SingleItemContextMatcher[Item, A2]): SingleItemContextMatcher[Item, A2] = SingleItemContextMatcher.Or(this, that)
	/** Operator version of `or`, specialized for SingleElementContextMatchers */
	def |[A2 >: A](that: SingleItemContextMatcher[Item, A2]): SingleItemContextMatcher[Item, A2] = SingleItemContextMatcher.Or(this, that)

	/** Creates a new single-element matcher which combines the results of both `this` matcher and `that` matcher.
	  * Both `this` and `that` will operate on the first element of the stack (as opposed to Chained matchers).
	  *
	  * @param that   The matcher to combine
	  * @param reduce The `TypeReduce` rule for combining the two match results
	  * @tparam A1 To satisfy covariance on A
	  * @tparam B The other matcher's result type
	  * @tparam R The combined result type
	  * @return A new matcher which combines the results of `this` and `that`
	  */
	def and[A1 >: A, B, R](that: SingleItemContextMatcher[Item, B])(implicit reduce: TypeReduce.Aux[A1, B, R]): SingleItemContextMatcher[Item, R] = {
		SingleItemContextMatcher.And(this, that)
	}
	/** Operator version of `and` */
	def &[A1 >: A, B, R](that: SingleItemContextMatcher[Item, B])(implicit reduce: TypeReduce.Aux[A1, B, R]): SingleItemContextMatcher[Item, R] = {
		SingleItemContextMatcher.And(this, that)
	}
}

object SingleItemContextMatcher {

	/** Create a new single-element matcher which calls the given matcher function `f` on the first
	  * element of the stack. If `f` returns `None`, the match fails, otherwise, the match succeeds.
	  *
	  * @param f The matcher function
	  * @tparam A The match result type
	  * @return A new single-element matcher which applies `f` to the stack head
	  */
	def apply[Item, A](f: Item => Option[A]): SingleItemContextMatcher[Item, A] = new Default(f)

	/** Create a new single-element matcher which calls the given matcher function `f` on the first
	  * element of the stack. If `f` returns `None`, the match fails, otherwise, the match succeeds.
	  *
	  * @param name This value will be used as the matcher's `toString`
	  * @param f    The matcher function
	  * @tparam A The match result type
	  * @return A new single-element matcher which applies `f` to the stack head
	  */
	def apply[Item, A](name: String, f: Item => Option[A]): SingleItemContextMatcher[Item, A] = new Default(f) {
		override def toString = name
	}

	/** Create a new single-element matcher which calls the given predicate function `f` on the first
	  * element of the stack. If `f` returns `true`, the match succeeds. Otherwise, the match fails.
	  *
	  * @param f The predicate function
	  * @return A new single-element matcher which uses `f` to determine a match
	  */
	def predicate[Item](f: Item => Boolean): SingleItemContextMatcher[Item, Unit] = new Predicate(f)

	/** Create a new single-element matcher which calls the given predicate function `f` on the first
	  * element of the stack. If `f` returns `true`, the match succeeds. Otherwise, the match fails.
	  *
	  * @param name This value will be used as the matcher's `toString`
	  * @param f    The predicate function
	  * @return A new single-element matcher which uses `f` to determine a match
	  */
	def predicate[Item](name: String, f: Item => Boolean): SingleItemContextMatcher[Item, Unit] = new Predicate(f) {
		override def toString = name
	}

	/** SingleElementContextMatcher that delegates to the given function `f` to perform its match.
	  *
	  * @param f A function that performs the context match on a single element
	  * @tparam A The matched context type
	  */
	class Default[Item, A](f: Item => Option[A]) extends SingleItemContextMatcher[Item, A] {
		def applyElem(elem: Item): Option[A] = f(elem)
	}

	/** SingleElementContextMatcher that successfully matches (with no result) if the
	  * given predicate function returns `true` for the matched element.
	  *
	  * @param f The predicate function
	  */
	class Predicate[Item](f: Item => Boolean) extends SingleItemContextMatcher[Item, Unit] {
		def applyElem(elem: Item): Option[Unit] = if (f(elem)) predicateSuccess else None
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
	case class And[Item, A, B, R](left: SingleItemContextMatcher[Item, A], right: SingleItemContextMatcher[Item, B])(implicit reduce: TypeReduce.Aux[A, B, R]) extends SingleItemContextMatcher[Item, R] {
		override def toString = s"($left & $right)"
		def applyElem(elem: Item): Option[R] = {
			for {
				a <- left.applyElem(elem)
				b <- right.applyElem(elem)
			} yield reduce(a, b)
		}
	}

	/** Similar to `ContextMatcher.Or`, but specialized for `SingleElementContextMatcher` */
	case class Or[Item, A](left: SingleItemContextMatcher[Item, A], right: SingleItemContextMatcher[Item, A]) extends SingleItemContextMatcher[Item, A] {
		override def toString = s"($left | $right)"
		def applyElem(elem: Item): Option[A] = left.applyElem(elem) orElse right.applyElem(elem)
	}

	/** Similar to `ContextMatcher.Mapped`, but specialized for `SingleElementContextMatcher` */
	case class Mapped[Item, A, B](inner: SingleItemContextMatcher[Item, A], op: String = "map")(f: A => Option[B]) extends SingleItemContextMatcher[Item, B] {
		override def toString = s"$inner.$op($f)"
		def applyElem(elem: Item): Option[B] = inner.applyElem(elem).flatMap(f)
		override def map[B2](g: B => B2): SingleItemContextMatcher[Item, B2] = Mapped(inner, s"$op+map") { a => f(a).map(g) }
		override def flatMap[B2](g: B => Option[B2]): SingleItemContextMatcher[Item, B2] = Mapped(inner, s"$op+flatMap") { a => f(a).flatMap(g) }
		override def filter(p: B => Boolean): SingleItemContextMatcher[Item, B] = Mapped(inner, s"$op+filter") { a => f(a).filter(p) }
	}
}
