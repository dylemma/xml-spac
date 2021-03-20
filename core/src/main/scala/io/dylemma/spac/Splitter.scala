package io.dylemma.spac

import cats.{Monad, MonadError}
import io.dylemma.spac.impl.{SplitterByContextMatch, SplitterJoiner}
import org.tpolecat.typename.TypeName

import scala.language.implicitConversions

trait Splitter[In, +C] {

	def addBoundaries: Transformer[In, Either[ContextChange[In, C], In]]

	def flatMap[Out](transformMatches: ContextPush[In, C] => Transformer[In, Out]): Transformer[In, Out] = addBoundaries >> SplitterJoiner(transformMatches)
	def mapTraced[Out](parseMatches: ContextPush[In, C] => Parser[In, Out]): Transformer[In, Out] = flatMap(parseMatches(_).asTransformer)
	def map[Out](parseMatches: C => Parser[In, Out]): Transformer[In, Out] = mapTraced(push => parseMatches(push.context))
	def joinBy[Out](parser: Parser[In, Out]): Transformer[In, Out] = map(_ => parser)
	def as[Out](implicit parser: Parser[In, Out]) = map(_ => parser)
}
object Splitter {
	def apply[In] = new SplitterApplyWithBoundInput[In]

	def fromMatcher[In, Elem, C](matcher: ContextMatcher[Elem, C])(implicit S: StackLike[In, Elem]): Splitter[In, C] = new ContextMatchSplitter(matcher)
}

class SplitterApplyWithBoundInput[In] {
	def fromMatcher[Elem, C](matcher: ContextMatcher[Elem, C])(implicit S: StackLike[In, Elem]): Splitter[In, C] = Splitter.fromMatcher(matcher)
}

class ContextMatchSplitter[In, Elem, C]
	(matcher: ContextMatcher[Elem, C])
	(implicit inAsStack: StackLike[In, Elem])
extends Splitter[In, C] {
	def addBoundaries: Transformer[In, Either[ContextChange[In, C], In]] = inAsStack.interpret >> SplitterByContextMatch(matcher)
}
