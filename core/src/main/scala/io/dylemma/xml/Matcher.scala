package io.dylemma.xml

import io.dylemma.xml.IterateeHelpers.OpenTag

trait Matcher[+A] extends ContextMatchSplitter[A] { self =>
	def apply(input: OpenTag): Option[A]

	def &[B, AB](that: Matcher[B])(implicit rc: ContextCombiner[A, B, AB]): Matcher[AB] = Matcher.combine(this, that)
	def /[B, AB](that: Matcher[B])(implicit rc: ContextCombiner[A, B, AB]): ListMatcher[AB] = {
		ListMatcher.inductive(ListMatcher.single(this), ListMatcher.single(that))
	}

	def matchContext(context: List[OpenTag]): Option[A] = context match {
		case Nil => None
		case head :: tail => apply(head)
	}
	def map[B](f: (A) => B): Matcher[B] = new Matcher[B] {
		def apply(input: OpenTag) = self.apply(input).map(f)
	}
}

trait ListMatcher[+A] extends ContextMatchSplitter[A] { self =>
	def apply(inputs: List[OpenTag]): Option[(A, List[OpenTag])]
	def /[B, AB](next: Matcher[B])(implicit rc: ContextCombiner[A, B, AB]): ListMatcher[AB] = {
		ListMatcher.inductive(this, ListMatcher.single(next))
	}

	def matchContext(context: List[OpenTag]): Option[A] = apply(context).map(_._1)
	def map[B](f: (A) => B): ListMatcher[B] = new ListMatcher[B] {
		def apply(inputs: List[OpenTag]) = self.apply(inputs).map{
			case (a, tail) => f(a) -> tail
		}
	}
}

object Matcher {
	def apply[A](f: OpenTag => Option[A]): Matcher[A] = new Matcher[A] {
		def apply(elem: OpenTag) = f(elem)
	}
	def predicate(f: OpenTag => Boolean): Matcher[Unit] = new Matcher[Unit] {
		def apply(elem: OpenTag) = if (f(elem)) Some(()) else None
	}
	def combine[A, B, AB](left: Matcher[A], right: Matcher[B])(implicit rc: ContextCombiner[A, B, AB]): Matcher[AB] = {
		new Matcher[AB] {
			def apply(input: OpenTag) = for(a <- left(input); b <- right(input)) yield rc.combine(a, b)
		}
	}
}

object ListMatcher {
	def single[A](matcher: Matcher[A]): ListMatcher[A] = new ListMatcher[A] {
		def apply(inputs: List[OpenTag]) = inputs match {
			case Nil => None
			case head :: tail => matcher(head).map(_ -> tail)
		}
	}

	def inductive[A, B, AB](leading: ListMatcher[A], next: ListMatcher[B])(
		implicit rc: ContextCombiner[A, B, AB]): ListMatcher[AB] = new ListMatcher[AB] {
		def apply(inputs: List[OpenTag]) = for {
			(leadMatch, leadTail) <- leading(inputs)
			(nextMatch, nextTail) <- next(leadTail)
		} yield rc.combine(leadMatch, nextMatch) -> nextTail
	}
}
