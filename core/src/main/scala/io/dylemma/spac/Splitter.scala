package io.dylemma.spac

import io.dylemma.spac.impl.{SplitterByConsecutiveMatches, SplitterByContextMatch, SplitterByInputMatch, SplitterJoiner}

trait Splitter[In, +C] {

	def addBoundaries: Transformer[In, Either[ContextChange[In, C], In]]

	def flatMap[Out](transformMatches: ContextPush[In, C] => Transformer[In, Out]): Transformer[In, Out] = addBoundaries :>> SplitterJoiner(transformMatches)
	def mapTraced[Out](parseMatches: ContextPush[In, C] => Parser[In, Out]): Transformer[In, Out] = flatMap(parseMatches(_).asTransformer)
	def map[Out](parseMatches: C => Parser[In, Out]): Transformer[In, Out] = mapTraced(push => parseMatches(push.context))
	def joinBy[Out](parser: Parser[In, Out]): Transformer[In, Out] = map(_ => parser)
	def as[Out](implicit parser: Parser[In, Out]) = map(_ => parser)
}
object Splitter {
	def apply[In] = new SplitterApplyWithBoundInput[In]

	def fromMatcher[In, Elem, C](matcher: ContextMatcher[Elem, C])(implicit S: StackLike[In, Elem]): Splitter[In, C] = new SplitterByContextMatch(matcher)

	def splitOnMatch[In, C](matcher: PartialFunction[In, C]): Splitter[In, C] = new SplitterByInputMatch(matcher)
	def splitOnMatch[In](f: In => Boolean): Splitter[In, Any] = splitOnMatch[In, Unit] { case in if f(in) => () }

	def consecutiveMatches[In, Context](matcher: PartialFunction[In, Context]): Splitter[In, Context] = new SplitterByConsecutiveMatches(matcher)
	def consecutiveMatches[In](p: In => Boolean): Splitter[In, Any] = consecutiveMatches[In, Any] { case in if p(in) => () }
}

class SplitterApplyWithBoundInput[In] {
	def fromMatcher[Elem, C](matcher: ContextMatcher[Elem, C])(implicit S: StackLike[In, Elem]): Splitter[In, C] = Splitter.fromMatcher(matcher)
	def splitOnMatch[C](matcher: PartialFunction[In, C]): Splitter[In, C] = Splitter.splitOnMatch(matcher)
	def splitOnMatch(f: In => Boolean): Splitter[In, Any] = Splitter.splitOnMatch(f)
	def consecutiveMatches[Context](matcher: PartialFunction[In, Context]): Splitter[In, Context] = Splitter.consecutiveMatches(matcher)
	def consecutiveMatches(p: In => Boolean): Splitter[In, Any] = Splitter.consecutiveMatches(p)
}
