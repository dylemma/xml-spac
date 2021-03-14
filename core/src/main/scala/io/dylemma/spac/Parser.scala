package io.dylemma.spac

import cats.data.Chain
import cats.effect.Bracket
import cats.{Applicative, Defer, Functor, Monad, MonadError}
import io.dylemma.spac.impl._
import io.dylemma.spac.types.Unconsable

import scala.collection.mutable
import scala.reflect.ClassTag

trait Parser[F[+_], -In, +Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]]
	def finish: F[Out]

	def map[Out2](f: Out => Out2)(implicit F: Functor[F]): Parser[F, In, Out2] = new ParserMapped(this, f)

	def orElse[In2 <: In, Out2 >: Out](fallback: Parser[F, In2, Out2])(implicit F: MonadError[F, Throwable]): Parser[F, In2, Out2] = ParserOrElseList(Right(this) :: Right(fallback) :: Nil)

	def withName(name: String)(implicit F: Functor[F]): Parser[F, In, Out] = new ParserNamed(name, this)

	def asTransformer(implicit F: Functor[F]): Transformer[F, In, Out] = new ParserAsTransformer(this)

	def stepMany[C[_], In2 <: In](inputs: C[In2])(implicit C: Unconsable[C], F: Monad[F]): F[Either[(Out, C[In2]), Parser[F, In, Out]]] = {
		F.tailRecM((inputs, this)) { case (pendingInputs, parser) =>
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

	def parse[Src](source: Src)(implicit srcToPullable: ToPullable[F, Src, In], F: Monad[F], Fb: Bracket[F, Throwable]): F[Out] = {
		srcToPullable(source).use { srcPullable =>
			F.tailRecM(srcPullable -> this) { case (inputs, parser) =>
				F.flatMap(inputs.uncons) {
					case Some((in, nextInputs)) =>
						F.map(parser.step(in)) {
							case Left(result) => Right(result)
							case Right(nextParser) => Left(nextInputs -> nextParser)
						}
					case None =>
						F.map(parser.finish)(Right(_))
				}
			}
		}
	}

	def parseSeq[C[_], In2 <: In](source: C[In2])(implicit srcAsSeq: Unconsable[C], F: Monad[F]): F[Out] = {
		F.tailRecM(source -> this) { case (inputs, parser) =>
			srcAsSeq.uncons(inputs) match {
				case Some((in, nextInputs)) =>
					F.map(parser.step(in)) {
						case Left(result) => Right(result)
						case Right(nextParser) => Left(nextInputs -> nextParser)
					}
				case None =>
					F.map(parser.finish)(Right(_))
			}
		}
	}
}

class ParserApplyWithBoundEffect[F[+_]] {
	def over[In]: ParserApplyBound[F, In] = new ParserApplyBound

	def app[In](implicit F: Monad[F]): Applicative[Parser[F, In, *]] = Parser.parserApplicative
	def firstOpt[In](implicit F: Applicative[F]): Parser[F, In, Option[In]] = Parser.firstOpt
	def firstOrError[In, Err](ifNone: Err)(implicit F: MonadError[F, Err]): Parser[F, In, In] = Parser.firstOrError(ifNone)
	def first[In](implicit F: MonadError[F, Throwable], tag: ClassTag[In]): Parser[F, In, In] = Parser.first
	def find[In](predicate: In => Boolean)(implicit F: Applicative[F]): Parser[F, In, Option[In]] = Parser.find(predicate)
	def findEval[In](predicate: In => F[Boolean])(implicit F: Monad[F]): Parser[F, In, Option[In]] = Parser.findEval(predicate)
	def fold[In, Out](init: Out)(op: (Out, In) => Out)(implicit F: Applicative[F]): Parser[F, In, Out] = Parser.fold(init)(op)
	def foldEval[In, Out](init: Out)(op: (Out, In) => F[Out])(implicit F: Monad[F]): Parser[F, In, Out] = Parser.foldEval(init)(op)
	def pure[Out](value: Out)(implicit F: Applicative[F]): Parser[F, Any, Out] = Parser.pure(value)
	def eval[In, Out](fa: F[Parser[F, In, Out]])(implicit F: Monad[F]): Parser[F, In, Out] = Parser.eval(fa)
	def toChain[In](implicit F: Applicative[F]): Parser[F, In, Chain[In]] = Parser.toChain
	def toList[In](implicit F: Applicative[F]): Parser[F, In, List[In]] = Parser.toList
	def impureBuild[In, Out](builder: => mutable.Builder[In, Out])(implicit fm: Monad[F], fd: Defer[F]): Parser[F, In, Out] = Parser.impureBuild(builder)
}

class ParserApplyBound[F[+_], In] {
	def app(implicit F: Monad[F]): Applicative[Parser[F, In, *]] = Parser.parserApplicative
	def firstOpt(implicit F: Applicative[F]): Parser[F, In, Option[In]] = Parser.firstOpt
	def firstOrError[Err](ifNone: Err)(implicit F: MonadError[F, Err]): Parser[F, In, In] = Parser.firstOrError(ifNone)
	def first(implicit F: MonadError[F, Throwable], tag: ClassTag[In]): Parser[F, In, In] = Parser.first
	def find(predicate: In => Boolean)(implicit F: Applicative[F]): Parser[F, In, Option[In]] = Parser.find(predicate)
	def findEval(predicate: In => F[Boolean])(implicit F: Monad[F]): Parser[F, In, Option[In]] = Parser.findEval(predicate)
	def fold[Out](init: Out)(op: (Out, In) => Out)(implicit F: Applicative[F]): Parser[F, In, Out] = Parser.fold(init)(op)
	def foldEval[Out](init: Out)(op: (Out, In) => F[Out])(implicit F: Monad[F]): Parser[F, In, Out] = Parser.foldEval(init)(op)
	def pure[Out](value: Out)(implicit F: Applicative[F]): Parser[F, In, Out] = Parser.pure(value)
	def eval[Out](fa: F[Parser[F, In, Out]])(implicit F: Monad[F]): Parser[F, In, Out] = Parser.eval(fa)
	def toChain(implicit F: Applicative[F]): Parser[F, In, Chain[In]] = Parser.toChain
	def toList(implicit F: Applicative[F]): Parser[F, In, List[In]] = Parser.toList
	def impureBuild[Out](builder: => mutable.Builder[In, Out])(implicit fm: Monad[F], fd: Defer[F]): Parser[F, In, Out] = Parser.impureBuild(builder)
}

class ParserApplyWithBoundInput[In] {
	def in[F[+_]]: ParserApplyBound[F, In] = new ParserApplyBound

	def app[F[+_]: Monad]: Applicative[Parser[F, In, *]] = Parser.parserApplicative
	def firstOpt[F[+_]: Applicative]: Parser[F, In, Option[In]] = Parser.firstOpt
	def firstOrError[F[+_], Err](ifNone: Err)(implicit F: MonadError[F, Err]): Parser[F, In, In] = Parser.firstOrError(ifNone)
	def first[F[+_]](implicit F: MonadError[F, Throwable], In: ClassTag[In]): Parser[F, In, In] = Parser.first
	def find[F[+_]: Applicative](predicate: In => Boolean): Parser[F, In, Option[In]] = Parser.find(predicate)
	def findEval[F[+_]: Monad](predicate: In => F[Boolean]): Parser[F, In, Option[In]] = Parser.findEval(predicate)
	def fold[F[+_]: Applicative, Out](init: Out)(op: (Out, In) => Out): Parser[F, In, Out] = Parser.fold(init)(op)
	def foldEval[F[+_]: Monad, Out](init: Out)(op: (Out, In) => F[Out]): Parser[F, In, Out] = Parser.foldEval(init)(op)
	def pure[F[+_]: Applicative, Out](value: Out): Parser[F, In, Out] = Parser.pure(value)
	def eval[F[+_]: Monad, Out](fa: F[Parser[F, In, Out]]): Parser[F, In, Out] = Parser.eval(fa)
	def toChain[F[+_]: Applicative]: Parser[F, In, Chain[In]] = Parser.toChain
	def toList[F[+_]: Applicative]: Parser[F, In, List[In]] = Parser.toList
	def impureBuild[F[+_]: Monad : Defer, Out](builder: => mutable.Builder[In, Out]): Parser[F, In, Out] = Parser.impureBuild(builder)
}

object Parser {

	def apply[F[+_]] = new ParserApplyWithBoundEffect[F]
	def apply[F[+_], In] = new ParserApplyBound[F, In]
	def over[In] = new ParserApplyWithBoundInput[In]

	def firstOpt[F[+_] : Applicative, In]: Parser[F, In, Option[In]] = new ParseFirstOpt[F, In]
	def firstOrError[F[+_], In, Err](ifNone: Err)(implicit F: MonadError[F, Err]): Parser[F, In, In] = firstOpt[F, In].errorIfNone(ifNone)
	def first[F[+_], In](implicit F: MonadError[F, Throwable], tag: ClassTag[In]): Parser[F, In, In] = firstOpt[F, In].errorIfNone[Throwable](new MissingFirstException[In])
	def find[F[+_] : Applicative, In](predicate: In => Boolean): Parser[F, In, Option[In]] = new ParserFind(predicate)
	def findEval[F[+_] : Monad, In](predicate: In => F[Boolean]): Parser[F, In, Option[In]] = new ParserFindEval(predicate)
	def fold[F[+_] : Applicative, In, Out](init: Out)(op: (Out, In) => Out): Parser[F, In, Out] = new ParserFold(init, op)
	def foldEval[F[+_] : Monad, In, Out](init: Out)(op: (Out, In) => F[Out]): Parser[F, In, Out] = new ParserFoldEval(init, op)

	def pure[F[+_] : Applicative, Out](value: Out): Parser[F, Any, Out] = new ParserPure(value)
	def eval[F[+_] : Monad, In, Out](fa: F[Parser[F, In, Out]]): Parser[F, In, Out] = new ParserEval(fa)

	def toChain[F[+_] : Applicative, In]: Parser[F, In, Chain[In]] = fold(Chain.empty[In])(_ :+ _)
	def toList[F[+_] : Applicative, In]: Parser[F, In, List[In]] = toChain[F, In].map(_.toList)

	def impureBuild[F[+_] : Monad : Defer, In, Out](builder: => mutable.Builder[In, Out]): Parser[F, In, Out] = {
		eval(Defer[F].defer { Monad[F].pure(new ParserImpureBuilder(builder)) })
	}

	implicit class OptionalParserOps[F[+_], -In, +Out](parser: Parser[F, In, Option[Out]]) {
		def errorIfNone[E](err: E)(implicit F: MonadError[F, E]): Parser[F, In, Out] = new ParserOptOrElse(parser, F.raiseError(err))
	}

	implicit def parserApplicative[F[+_] : Monad, In]: Applicative[Parser[F, In, *]] = new Applicative[Parser[F, In, *]] {
		def pure[A](x: A) = new ParserPure(x)
		def ap[A, B](ff: Parser[F, In, A => B])(fa: Parser[F, In, A]) = product(ff, fa).map { case (f, a) => f(a) }
		override def product[A, B](fa: Parser[F, In, A], fb: Parser[F, In, B]) = {
			(fa, fb) match {
				case (faCompound: ParserCompoundN[F, In, A], fbCompound: ParserCompoundN[F, In, B]) =>
					fbCompound.compoundProductWithLhs(faCompound)
				case (fa, fbCompound: ParserCompoundN[F, In, B]) =>
					fbCompound.productWithLhs(fa)
				case (faCompound: ParserCompoundN[F, In, A], fb) =>
					faCompound.productWithRhs(fb)
				case (fa, fb) =>
					new ParserCompoundN(
						List(1 -> fa, 0 -> fb),
						Map.empty,
						results => (results(1).asInstanceOf[A], results(0).asInstanceOf[B])
					)
			}
		}
		override def map[A, B](fa: Parser[F, In, A])(f: A => B): Parser[F, In, B] = fa.map(f)
	}
}