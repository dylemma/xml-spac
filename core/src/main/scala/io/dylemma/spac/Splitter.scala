package io.dylemma.spac

import cats.{Monad, MonadError}
import io.dylemma.spac.impl.{SplitterJoiner, StackMatchSplitter}
import io.dylemma.spac.types.Stackable2

import scala.language.implicitConversions

trait Splitter[F[+_], In, C] {
	def addBoundaries: Transformer[F, In, Either[ContextChange[In, C], In]]

	def flatMap[Out](transformMatches: ContextPush[In, C] => Transformer[F, In, Out])(implicit F: MonadError[F, Throwable]): Transformer[F, In, Out] = {
		addBoundaries >> SplitterJoiner(transformMatches)
	}

	def map[Out](parseMatches: ContextPush[In, C] => Parser[F, In, Out])(implicit F: MonadError[F, Throwable]): Transformer[F, In, Out] = {
		flatMap(parseMatches(_).asTransformer)
	}

	/** Convenience alias for `map` which lets you pass the parser implicitly in cases where the context value is ignored */
	def as = new SplitterConsumerOps(this, ConsumerK.identity[F])
	def first(implicit F: MonadThrow[F]) = new SplitterConsumerOps(this, ConsumerK.first[F])
	def firstOption(implicit F: Monad[F]) = new SplitterConsumerOps(this, ConsumerK.firstOption[F])
	def asList(implicit F: Monad[F]) = new SplitterConsumerOps(this, ConsumerK.toList[F])
}
object Splitter {
	def apply[F[+_], C] = new SplitterApplyBound[F, C]
	implicit def consume[F[+_], In, C](splitter: Splitter[F, In, C]) = new SplitterConsumerOps(splitter, ConsumerK.identity[F])

	def fromMatcher[F[+_], In, Elem, C](matcher: ContextMatcher[Elem, C])(implicit F: Monad[F], S: Stackable2[In, Elem]): Splitter[F, In, C] = new ContextMatchSplitter(matcher)
}

class SplitterApplyBound[F[+_], In] {
	def apply[Elem, C](matcher: ContextMatcher[Elem, C])(implicit F: Monad[F], S: Stackable2[In, Elem]): Splitter[F, In, C] = Splitter.fromMatcher(matcher)
}

class ContextMatchSplitter[F[+_], In, Elem, C]
	(matcher: ContextMatcher[Elem, C])
	(implicit inAsStack: Stackable2[In, Elem], F: Monad[F])
extends Splitter[F, In, C] {
	def addBoundaries: Transformer[F, In, Either[ContextChange[In, C], In]] = inAsStack.interpret[F] >> StackMatchSplitter(matcher)
}

class SplitterConsumerOps[F[+_], In, P[_[+_], _, _], Ev[_], C](
	val self: Splitter[F, In, C],
	val consumerK: ConsumerK[F, P, Ev]
) {
	def apply[Out](implicit parser: Parser[F, In, Out], F: MonadThrow[F], ev: Ev[Out]): P[F, In, Out] = consumerK(self.map(_ => parser))
	def into[Out](parser: ContextPush[In, C] => Parser[F, In, Out])(implicit F: MonadThrow[F], ev: Ev[Out]): P[F, In, Out] = consumerK(self.map(parser))
}