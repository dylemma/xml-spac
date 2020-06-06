package io.dylemma.spac.impl

import cats.data.Chain
import cats.implicits._
import cats.{Applicative, Functor, Monad}
import io.dylemma.spac._

class ParserAsTransformer[F[+_], -In, +Out](parser: Parser[F, In, Out])(implicit F: Functor[F]) extends Transformer[F, In, Out] {
	def step(in: In) = F.map(parser.step(in)) {
		case Left(out) => Chain.one(out) -> None
		case Right(nextParser) =>
			val nextTransform = if(nextParser eq parser) this else new ParserAsTransformer(nextParser)
			Chain.nil -> Some(nextTransform)
	}
	def finish = F.map(parser.finish) { Chain.one }
}

class ParseFirstOpt[F[+_], In](implicit F: Applicative[F]) extends Parser[F, In, Option[In]] {
	def step(in: In): F[Either[Option[In], Parser[F, In, Option[In]]]] = F.pure(Left(Some(in)))
	def finish: F[Option[In]] = F.pure(None)
}

class ParserOptOrElse[F[+_], -In, +Out](parser: Parser[F, In, Option[Out]], resultIfNone: F[Out])(implicit F: Monad[F]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = F.flatMap(parser.step(in)) {
		case Left(Some(out)) => F.pure(Left(out))
		case Left(None) => F.map(resultIfNone)(Left(_))
		case Right(`parser`) => F.pure(Right(this))
		case Right(nextParser) => F.pure(Right(new ParserOptOrElse(nextParser, resultIfNone)))
	}
	def finish: F[Out] = F.flatMap(parser.finish) {
		case Some(out) => F.pure(out)
		case None => resultIfNone
	}
}

class ParserFind[F[+_], In](predicate: In => Boolean)(implicit F: Applicative[F]) extends Parser[F, In, Option[In]] {
	def step(_in: In) = F.map(F.pure(_in)){ in =>
		if(predicate(in)) Left(Some(in))
		else Right(this)
	}
	def finish = F.pure(None)
}

class MappedParser[F[+_], In, Out, Out2](inner: Parser[F, In, Out], f: Out => Out2)(implicit F: Functor[F]) extends Parser[F, In, Out2] {
	def step(in: In): F[Either[Out2, Parser[F, In, Out2]]] = F.map(inner.step(in)) {
		case Left(out) => Left(f(out))
		case Right(`inner`) => Right(this)
		case Right(nextInner) => Right(new MappedParser(nextInner, f))
	}
	def finish: F[Out2] = F.map(inner.finish)(f)
}

class ParserFold[F[+_], In, Out](state: Out, fold: (Out, In) => Out)(implicit F: Applicative[F]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = in.pure[F].map { x => Right(new ParserFold(fold(state, x), fold)) }
	def finish: F[Out] = F.pure(state)
}