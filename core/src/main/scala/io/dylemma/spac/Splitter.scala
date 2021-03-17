package io.dylemma.spac

import cats.arrow.FunctionK
import cats.{Monad, MonadError}
import io.dylemma.spac.impl.{SplitterJoiner, SplitterByContextMatch}
import org.tpolecat.typename.TypeName

import scala.language.implicitConversions
import scala.reflect.ClassTag

trait Splitter[F[+_], In, +C] {
	def addBoundaries: Transformer[F, In, Either[ContextChange[In, C], In]]

	def flatMap[Out](transformMatches: ContextPush[In, C] => Transformer[F, In, Out])(implicit F: MonadError[F, Throwable]): Transformer[F, In, Out] = {
		addBoundaries >> SplitterJoiner(transformMatches)
	}

	def map[Out](parseMatches: ContextPush[In, C] => Parser[F, In, Out])(implicit F: MonadError[F, Throwable]): Transformer[F, In, Out] = {
		flatMap(parseMatches(_).asTransformer)
	}

	def as[Out](implicit parser: Parser[F, In, Out], F: MonadError[F, Throwable]) = map(_ => parser)
}
object Splitter {
	def apply[F[+_], C] = new SplitterApplyBound[F, C]

	def fromMatcher[F[+_], In, Elem, C](matcher: ContextMatcher[Elem, C])(implicit F: Monad[F], S: StackLike[In, Elem]): Splitter[F, In, C] = new ContextMatchSplitter(matcher)

	implicit class SplitterConvenienceWords[F[+_], In, C](splitter: Splitter[F, In, C])(implicit F: MonadError[F, Throwable]) {
		def first: SplitterWordFirst[F, In, C] = new SplitterWordFirst(splitter)
		def firstOpt: SplitterWordFirstOpt[F, In, C] = new SplitterWordFirstOpt(splitter)
		def asList: SplitterWordAsList[F, In, C] = new SplitterWordAsList(splitter)
	}

	class SplitterWordFirst[F[+_], In, +C](splitter: Splitter[F, In, C])(implicit F: MonadError[F, Throwable]) {
		def apply[Out](implicit parser: Parser[F, In, Out], Out: TypeName[Out]): Parser[F, In, Out] = splitter.map(_ => parser) :> Parser.first
		def into[Out](getParser: ContextPush[In, C] => Parser[F, In, Out])(implicit Out: TypeName[Out]): Parser[F, In, Out] = splitter.map(getParser) :> Parser.first
	}
	class SplitterWordFirstOpt[F[+_], In, +C](splitter: Splitter[F, In, C])(implicit F: MonadError[F, Throwable]) {
		def apply[Out](implicit parser: Parser[F, In, Out]): Parser[F, In, Option[Out]] = splitter.map(_ => parser) :> Parser.firstOpt
		def into[Out](getParser: ContextPush[In, C] => Parser[F, In, Out]): Parser[F, In, Option[Out]] = splitter.map(getParser) :> Parser.firstOpt
	}
	class SplitterWordAsList[F[+_], In, +C](splitter: Splitter[F, In, C])(implicit F: MonadError[F, Throwable]) {
		def apply[Out](implicit parser: Parser[F, In, Out]): Parser[F, In, List[Out]] = splitter.map(_ => parser) :> Parser.toList
		def into[Out](getParser: ContextPush[In, C] => Parser[F, In, Out]): Parser[F, In, List[Out]] = splitter.map(getParser) :> Parser.toList
	}
}

class SplitterApplyBound[F[+_], In] {
	def apply[Elem, C](matcher: ContextMatcher[Elem, C])(implicit F: Monad[F], S: StackLike[In, Elem]): Splitter[F, In, C] = Splitter.fromMatcher(matcher)
}

class ContextMatchSplitter[F[+_], In, Elem, C]
	(matcher: ContextMatcher[Elem, C])
	(implicit inAsStack: StackLike[In, Elem], F: Monad[F])
extends Splitter[F, In, C] {
	def addBoundaries: Transformer[F, In, Either[ContextChange[In, C], In]] = inAsStack.interpret[F] >> SplitterByContextMatch(matcher)
}
