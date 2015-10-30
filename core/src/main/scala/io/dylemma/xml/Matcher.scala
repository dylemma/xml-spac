package io.dylemma.xml

import javax.xml.namespace.QName
import scala.language.implicitConversions

import io.dylemma.xml.iteratee.IterateeHelpers.OpenTag

object TagMatcherSemantics extends MatcherSemantics[OpenTag] {
	import event._

	def tag(name: String) = Matcher.predicate {
		case OpenTag(Name(n), _) => name == n
	}
	def attr(name: String) = Matcher { tag =>
		val qname = new QName(name)
		tag.attrs get qname
	}

	implicit def matcherAsListMatcher[M <: MatchResult[_]](matcher: Matcher[M]): ListMatcher[M] = matcher.asListMatcher
	def main(args: Array[String]) {
		val matcher1 = tag("finding") & attr("id") & attr("stuff")
		val matcher2 = tag("foo")
		val matcher3 = tag("toolResult") & attr("id")

		val listMatcher = matcher1 / matcher2 / matcher3

		def Tag(name: String, attrs: (String, String)*) = {
			val qName = new QName(name)
			val attrsMap = (for {(k, v) <- attrs} yield (new QName(k), v)).toMap
			OpenTag(qName, attrsMap)
		}

		val tag1 = Tag("finding", "id" -> "123", "stuff" -> "foof", "things" -> "blarg")
		val tag2 = Tag("foo", "id" -> "asdf")
		val tag3 = Tag("toolResult", "id" -> "abc", "stuff" -> "eokjd")
		val tag4 = Tag("things", "content" -> "bad")
		val tag5 = Tag("thing", "format" -> "markdown")

		val tagsList = tag1 :: tag2 :: tag3 :: tag4 :: tag5 :: Nil
		val result = listMatcher(tagsList)
		println(result)

		val wrongList = tag1 :: tag2 :: tag5 :: Nil
		println(listMatcher(wrongList))

		val wrongList2 = tag1 :: Nil
		println(listMatcher(wrongList2))
	}
}

trait MatcherSemantics[Input] {

	trait Matcher[M <: MatchResult[_]] {
		def apply(input: Input): Option[M]

		def &[N <: MatchResult[_], R <: MatchResult[_]](that: Matcher[N])(
			implicit combiner: MatchResultCombiner[M, N, R]
		): Matcher[R] = new CombinedMatcher(this, that)

		def asListMatcher: ListMatcher[M] = new SingleListMatcher[M](this)

		def /[N <: MatchResult[_], MN <: MatchResult[_]](nextMatcher: Matcher[N])(
			implicit combiner: MatchResultCombiner[M, N, MN]): ListMatcher[MN] = {
			asListMatcher / nextMatcher
		}
	}

	private class CombinedMatcher[A <: MatchResult[_], B <: MatchResult[_], R <: MatchResult[_]]
		(a: Matcher[A], b: Matcher[B])
			(implicit combiner: MatchResultCombiner[A, B, R])
		extends Matcher[R] {
		def apply(input: Input): Option[R] = {
			for {
				resultA <- a(input)
				resultB <- b(input)
			} yield combiner.combine(resultA, resultB)
		}
	}

	object Matcher {
		import MatchResult._

		def apply[A](f: Input => Option[A]): Matcher[SingleValue[A]] = new Matcher[SingleValue[A]] {
			def apply(elem: Input) = f(elem) map {SingleValue(_)}
		}
		def predicate(f: Input => Boolean): Matcher[Ok.type] = new Matcher[Ok.type] {
			def apply(elem: Input) = if (f(elem)) Some(Ok) else None
		}
	}

	trait ListMatcher[M <: MatchResult[_]] {
		/** Consumes some number of inputs from the beginning of the `inputs` list,
			* optionally returning a result value and the remaining inputs as a pair.
			*
			* @param inputs
			* @return
			*/
		def apply(inputs: List[Input]): Option[(M, List[Input])]

		def /[N <: MatchResult[_], MN <: MatchResult[_]](nextMatcher: Matcher[N])(
			implicit combiner: MatchResultCombiner[M, N, MN]): ListMatcher[MN] = {
			new InductiveListMatcher(this, nextMatcher.asListMatcher)
		}
	}

	class SingleListMatcher[H <: MatchResult[_]](headMatcher: Matcher[H]) extends ListMatcher[H] {
		def apply(inputs: List[Input]) = inputs match {
			case Nil => None
			case head :: tail =>
				val headMatch = headMatcher(head)
				headMatch.map(_ -> tail)
		}
	}

	class InductiveListMatcher[A <: MatchResult[_], B <: MatchResult[_], AB <: MatchResult[_]]
		(leadMatcher: ListMatcher[A], secondMatcher: ListMatcher[B])
			(implicit combiner: MatchResultCombiner[A, B, AB])
		extends ListMatcher[AB] {

		def apply(inputs: List[Input]) = {
			for {
				(leadMatch, leadTail) <- leadMatcher(inputs)
				(secondMatch, remaining) <- secondMatcher(leadTail)
			} yield {
				val combinedMatch = combiner.combine(leadMatch, secondMatch)
				combinedMatch -> remaining
			}
		}
	}

	/*
	Matcher[A] / Matcher[B] / Matcher[C]
	thingA :: thingB :: thingC :: otherThings :: Nil
	= (Matcher[A] / Matcher[B]) / Matcher[C]
	= Matcher[AB] / Matcher[C]
	= Matcher[ABC]

	(Input, Matcher[A])
	(Input, Matcher[B])

	def apply(inputs: List[Input]) = inputs match {
		case Nil => fail
		case head :: tail =>
			val headResult = headMatcher(head)
			tailMatcher(tail) match {
				case tailResult + remainingStuff =>
					val ab = combine(headResult, tailResult)
					tailResult.fold(ab)

	 */
}