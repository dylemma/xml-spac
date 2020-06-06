package io.dylemma.spac

import cats.data.Chain
import cats.{Applicative, Functor, Monad, MonadError}
import io.dylemma.spac.impl._
import io.dylemma.spac.types.Unconsable

import scala.reflect.ClassTag

trait Parser[F[+_], -In, +Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]]
	def finish: F[Out]

	def map[Out2](f: Out => Out2)(implicit F: Functor[F]): Parser[F, In, Out2] = new MappedParser(this, f)
	def asTransformer(implicit F: Functor[F]): Transformer[F, In, Out] = new ParserAsTransformer(this)

	def stepMany[C[_], In2 <: In](inputs: C[In2])(implicit C: Unconsable[C], F: Monad[F]): F[Either[(Out, C[In2]), Parser[F, In, Out]]] = {
		F.tailRecM((inputs, this)){ case (pendingInputs, parser) =>
			C.uncons(pendingInputs) match {
				// end of the `inputs`, exit recursion with the current parser state
				case None => F.pure(Right(Right(parser)))

				// next input, step the parser
				case Some((head, tail)) =>
					F.map(parser.step(head)) {
						// parser finished with a result; return it along with the leftover inputs
						case Left(out) => Right(Left(out -> tail))
						// continue with an updated parser state
						case Right(nextParser) => Left(tail -> nextParser)
					}
			}
		}
	}
}

class ParserApplyWithBoundEffect[F[+_]] {
	def firstOpt[In](implicit F: Applicative[F]): Parser[F, In, Option[In]] = Parser.firstOpt
	def firstOrError[In, Err](ifNone: Err)(implicit F: MonadError[F, Err]): Parser[F, In, In] = Parser.firstOrError(ifNone)
	def first[In](implicit F: MonadError[F, Throwable], tag: ClassTag[In]): Parser[F, In, In] = Parser.first
	def find[In](predicate: In => Boolean)(implicit F: Applicative[F]): Parser[F, In, Option[In]] = Parser.find(predicate)
	def fold[In, Out](init: Out)(op: (Out, In) => Out)(implicit F: Applicative[F]): Parser[F, In, Out] = Parser.fold(init)(op)
	def toChain[In](implicit F: Applicative[F]): Parser[F, In, Chain[In]] = fold(Chain.empty[In])(_ :+ _)
	def toList[In](implicit F: Applicative[F]): Parser[F, In, List[In]] = toChain[In].map(_.toList)
}

class ParserApplyBound[F[+_], In] {
	def firstOpt(implicit F: Applicative[F]): Parser[F, In, Option[In]] = Parser.firstOpt
	def firstOrError[Err](ifNone: Err)(implicit F: MonadError[F, Err]): Parser[F, In, In] = Parser.firstOrError(ifNone)
	def first(implicit F: MonadError[F, Throwable], tag: ClassTag[In]): Parser[F, In, In] = Parser.first
	def find(predicate: In => Boolean)(implicit F: Applicative[F]): Parser[F, In, Option[In]] = Parser.find(predicate)
	def fold[Out](init: Out)(op: (Out, In) => Out)(implicit F: Applicative[F]): Parser[F, In, Out] = Parser.fold(init)(op)
	def toChain(implicit F: Applicative[F]): Parser[F, In, Chain[In]] = fold(Chain.empty[In])(_ :+ _)
	def toList(implicit F: Applicative[F]): Parser[F, In, List[In]] = toChain.map(_.toList)
}

object Parser {

	def apply[F[+_]] = new ParserApplyWithBoundEffect[F]
	def apply[F[+_], In] = new ParserApplyBound[F, In]

	def firstOpt[F[+_]: Applicative, In]: Parser[F, In, Option[In]] = new ParseFirstOpt[F, In]
	def firstOrError[F[+_], In, Err](ifNone: Err)(implicit F: MonadError[F, Err]): Parser[F, In, In] = firstOpt[F, In].errorIfNone(ifNone)
	def first[F[+_], In](implicit F: MonadError[F, Throwable], tag: ClassTag[In]): Parser[F, In, In] = firstOpt[F, In].errorIfNone[Throwable](new MissingFirstException[In])
	def find[F[+_]: Applicative, In](predicate: In => Boolean): Parser[F, In, Option[In]] = new ParserFind(predicate)
	def fold[F[+_]: Applicative, In, Out](init: Out)(op: (Out, In) => Out): Parser[F, In, Out] = new ParserFold(init, op)

	def toChain[F[+_]: Applicative, In]: Parser[F, In, Chain[In]] = fold(Chain.empty[In])(_ :+ _)
	def toList[F[+_]: Applicative, In]: Parser[F, In, List[In]] = toChain[F, In].map(_.toList)

	implicit class OptionalParserOps[F[+_], -In, +Out](parser: Parser[F, In, Option[Out]]) {
		def errorIfNone[E](err: E)(implicit F: MonadError[F, E]): Parser[F, In, Out] = new ParserOptOrElse(parser, F.raiseError(err))
	}

}