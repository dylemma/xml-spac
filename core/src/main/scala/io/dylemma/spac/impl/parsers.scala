package io.dylemma.spac.impl

import cats.data.Chain
import cats.implicits._
import cats.{Applicative, FlatMap, Functor, Monad}
import io.dylemma.spac._

import scala.collection.mutable

class ParserPure[F[+_]: Applicative, Out](value: Out) extends Parser[F, Any, Out] {
	def step(in: Any): F[Either[Out, Parser[F, Any, Out]]] = Left(value).pure[F]
	def finish: F[Out] = value.pure[F]
}

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

class ParserFindEval[F[+_]: Monad, In](predicate: In => F[Boolean]) extends Parser[F, In, Option[In]] {
	def step(_in: In) = _in.pure[F].flatMap { in =>
		predicate(in).map {
			case true => Left(Some(in))
			case false => Right(this)
		}
	}
	def finish = None.pure[F]
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

class ParserImpureBuilder[F[+_], In, To](val builder: mutable.Builder[In, To])(implicit F: Applicative[F]) extends Parser[F, In, To] {
	def step(in: In) = F.pure(this).map { self =>
		self.builder += in
		Right(self)
	}
	def finish: F[To] = F.pure(this).map { _.builder.result() }
}

class ParserFoldEval[F[+_]: Monad, In, Out](state: Out, fold: (Out, In) => F[Out]) extends Parser[F, In, Out] {
	def step(in: In) = in.pure[F].flatMap { fold(state, _) }.map { nextState => Right(new ParserFoldEval(nextState, fold)) }
	def finish = state.pure[F]
}

class ParserEval[F[+_]: Monad, In, Out](init: F[Parser[F, In, Out]]) extends Parser[F, In, Out] {
	def step(in: In) = init.flatMap(_ step in)
	def finish: F[Out] = init.flatMap(_.finish)
}

class ParserTupled[F[+_]: Applicative, In, A, B](
	parserA: Either[A, Parser[F, In, A]],
	parserB: Either[B, Parser[F, In, B]]
) extends Parser[F, In, (A, B)] {

	def finish: F[(A, B)] = Applicative[F].tuple2(finishOne(parserA), finishOne(parserB))

	private def finishOne[X](parser: Either[X, Parser[F, In, X]]): F[X] = parser match {
		case Left(x) => x.pure[F]
		case Right(p) => p.finish
	}

	def step(in: In): F[Either[(A, B), Parser[F, In, (A, B)]]] = Applicative[F].map2(stepOne(parserA, in), stepOne(parserB, in)){
		case (Left(a), Left(b)) => Left((a, b)) // done
		case (contA, contB) => Right(new ParserTupled(contA, contB)) // doesn't matter which one is unfinished, we have to continue
	}
	private def stepOne[X](parser: Either[X, Parser[F, In, X]], in: In): F[Either[X, Parser[F, In, X]]] = parser match {
		case Left(x) => Left(x).pure[F]
		case Right(p) => p.step(in)
	}
}

class ParserCompoundN[F[+_]: Monad, In, Out](
	private val unfinished: List[(Int, Parser[F, In, Any])],
	private val finished: Map[Int, Any],
	private val assemble: (Int => Any) => Out
) extends Parser[F, In, Out] {

	def finish: F[Out] = {
		unfinished.map { case (i, parser) =>
			parser.finish.map(i -> _)
		}.sequence.map { justFinished =>
			assemble(finished ++ justFinished)
		}
	}
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = {
		unfinished.partitionEitherM { case (i, parser) =>
			parser.step(in).map {
				case Left(out) => Left(i -> out)
				case Right(cont) => Right(i -> cont)
			}
		}.map { case (addedFinished, newUnfinished) =>
			if (newUnfinished.isEmpty) {
				// all inner parsers have finished, so we can produce a result
				Left(assemble(finished ++ addedFinished))
			} else {
				// some inner parsers are still running, so continue with the remaining unfinished parsers
				Right(new ParserCompoundN(newUnfinished, finished ++ addedFinished, assemble))
			}
		}
	}

	def ap[Out2](ff: ParserCompoundN[F, In, Out => Out2]): ParserCompoundN[F, In, Out2] = {
		val offset = unfinished.size + finished.size
		val ffUnfinished = ff.unfinished.map { case (i, p) => (i + offset, p) }
		val ffFinished = ff.finished.map { case (i, r) => (i + offset, r) }
		val combinedAssemble = (get: Int => Any) => {
			val out = assemble(get)
			val f = ff.assemble(i => get(i + offset))
			f(out)
		}
		new ParserCompoundN(unfinished ++ ffUnfinished, finished ++ ffFinished, combinedAssemble)
	}
}