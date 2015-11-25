package io.dylemma.xml

import io.dylemma.xml.IterateeHelpers.OpenTag
import io.dylemma.xml.Result._

trait Matcher[+A] extends MapR[A, Matcher] with ContextMatchSplitter[A] { self =>
	def apply(input: OpenTag): Result[A]

	def &[B, AB](that: Matcher[B])(implicit rc: ContextCombiner[A, B, AB]): Matcher[AB] = Matcher.combine(this, that)
	def /[B, AB](that: Matcher[B])(implicit rc: ContextCombiner[A, B, AB]): ListMatcher[AB] = {
		ListMatcher.inductive(ListMatcher.single(this), ListMatcher.single(that))
	}

	def matchContext(context: List[OpenTag]): Result[A] = context match {
		case Nil => Empty
		case head :: tail => apply(head)
	}

	def mapR[B](f: Result[A] => Result[B]): Matcher[B] = new Matcher[B] {
		def apply(input: OpenTag) = f(self.apply(input))
	}

	/** Creates a tag matcher that, as long as this matcher gets a successful match,
		* will throw out the successful result and replace it with the name of the
		* element being matched.
		*/
	def extractName: Matcher[String] = new Matcher[String] {
		def apply(input: OpenTag) = {
			val innerResult = self(input)
			// throw out the actual result and return the tag's name
			innerResult map { _ => input.name.getLocalPart }
		}
	}

	/** Creates a tag matcher that will additionally return the name of the matched tag.
		* Uses `ContextCombiner` rules to determine the returned matcher's type by combining
		* this matcher's type with `String` (the name of the tag).
		*
		* @param rc The result combiner for this matcher's type and String
		* @tparam AB The output type when combining `A` and `String`
		*/
	def andExtractName[AB](implicit rc: ContextCombiner[A, String, AB]): Matcher[AB] = new Matcher[AB] {
		def apply(input: OpenTag) = {
			val innerResult = self(input)
			val nameResult = Result(input.name.getLocalPart)
			for(a <- innerResult; b <- nameResult) yield rc.combine(a, b)
		}
	}
}

trait ListMatcher[+A] extends MapR[A, ListMatcher] with ContextMatchSplitter[A] { self =>
	def apply(inputs: List[OpenTag]): Result[(A, List[OpenTag])]
	def /[B, AB](next: Matcher[B])(implicit rc: ContextCombiner[A, B, AB]): ListMatcher[AB] = {
		ListMatcher.inductive(this, ListMatcher.single(next))
	}

	def matchContext(context: List[OpenTag]): Result[A] = apply(context).map(_._1)

	def mapR[B](f: Result[A] => Result[B]): ListMatcher[B] = new ListMatcher[B] {
		def apply(inputs: List[OpenTag]) = {
			val selfResult = self(inputs)
			val aResult = selfResult.map(_._1)
			val tailResult = selfResult.map(_._2)
			val bResult = f(aResult)
			for(b <- bResult; tail <- tailResult) yield b -> tail
		}
	}
}

object Matcher {
	def apply[A](f: OpenTag => Option[A]): Matcher[A] = new Matcher[A] {
		def apply(elem: OpenTag) = Result fromOption { f(elem) }
	}
	def predicate(f: OpenTag => Boolean): Matcher[Unit] = new Matcher[Unit] {
		def apply(elem: OpenTag) = Success(()).withFilter(_ => f(elem))
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
			case Nil => Empty
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
