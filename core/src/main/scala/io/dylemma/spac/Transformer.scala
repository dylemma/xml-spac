package io.dylemma.spac

import cats.arrow.FunctionK
import cats.data.Chain
import cats.{Applicative, Functor, Monad}
import io.dylemma.spac.impl._
import io.dylemma.spac.types.Unconsable

trait Transformer[F[+_], -In, +Out] {
	def step(in: In): F[(Emit[Out], Option[Transformer[F, In, Out]])]
	def finish: F[Emit[Out]]

	def mapBatch[Out2](f: Emit[Out] => Emit[Out2])(implicit F: Functor[F]): Transformer[F, In, Out2] = new TransformerMapBatch(this, f)
	def map[Out2](f: Out => Out2)(implicit F: Functor[F]): Transformer[F, In, Out2] = mapBatch(_.map(f))
	def filter(predicate: Out => Boolean)(implicit F: Functor[F]): Transformer[F, In, Out] = mapBatch(_.filter(predicate))
	def collect[Out2](pf: PartialFunction[Out, Out2])(implicit F: Functor[F]): Transformer[F, In, Out2] = mapBatch(_.collect(pf))

	def through[Out2](next: Transformer[F, Out, Out2])(implicit F: Monad[F]): Transformer[F, In, Out2] = new TransformerPipe(this, next)
	def >>[Out2](next: Transformer[F, Out, Out2])(implicit F: Monad[F]): Transformer[F, In, Out2] = through(next)

	def into[Out2](parser: Parser[F, Out, Out2])(implicit F: Monad[F]): Parser[F, In, Out2] = new TransformedParser(this, parser)
	def :>[Out2](parser: Parser[F, Out, Out2])(implicit F: Monad[F]): Parser[F, In, Out2] = into(parser)

	def stepMany[C[_], In2 <: In](_ins: C[In2])(implicit C: Unconsable[C], F: Monad[F]): F[(Emit[Out], Either[C[In2], Transformer[F, In, Out]])] = {
		// fold inputs from `_ins` into this transformer, accumulating a buffer of `outs` and updating the transformer state along the way,
		// eventually returning the concatenation of all `outs`, and the final `transformer` state if the transformer is ready to continue,
		// or else the leftover unconsumed inputs if the transformer decided to end partway through
		F.tailRecM((_ins, Chain.empty[Out], this)){ case (ins, outs, transformer) =>
			C.uncons(ins) match {
				// end of input, exit recursion by returning a Right with the result
				case None => F.pure(Right(outs -> Right(transformer)))

				// next input, step the transformer
				case Some((in, remainingIn)) =>
					F.map(transformer.step(in)) {
						// transformer ended as a result of the input; return the leftover inputs
						case (moreOuts, None) => Right((outs ++ moreOuts) -> Left(remainingIn))
						// continue recursion
						case (moreOuts, Some(nextTransformer)) => Left((remainingIn, outs ++ moreOuts, nextTransformer))
					}
			}
		}
	}
}

object Transformer {
	def apply[F[+_], In] = new TransformerApplyBound[F, In]
	def apply[F[+_]] = new TransformerApplyWithBoundEffect[F]

	def op[F[+_]: Applicative, In, Out](f: In => Emit[Out]): Transformer[F, In, Out] = new TransformerOp(f)
	def map[F[+_]: Applicative, In, Out](f: In => Out): Transformer[F, In, Out] = op { in => Emit.one(f(in)) }
	def filter[F[+_]: Applicative, In](f: In => Boolean): Transformer[F, In, In] = op { in => if (f(in)) Emit.one(in) else Emit.nil }

	def take[F[+_]: Applicative, In](n: Int): Transformer[F, In, In] = new TransformerTake(n)

	type IntoParser[F[+_], In, R[_]] = FunctionK[Transformer[F, In, *], Lambda[A => Parser[F, In, R[A]]]]
	def intoListParser[F[+_], In]: IntoParser[F, In, List] = new IntoParser[F, In, List] {
		def apply[A](fa: Transformer[F, In, A]): Parser[F, In, List[A]] = ???
	}
}

class TransformerApplyBound[F[+_], In] {
	def op[Out](f: In => Emit[Out])(implicit F: Applicative[F]): Transformer[F, In, Out] = Transformer.op(f)
	def map[Out](f: In => Out)(implicit F: Applicative[F]): Transformer[F, In, Out] = Transformer.map(f)
	def filter(f: In => Boolean)(implicit F: Applicative[F]): Transformer[F, In, In] = Transformer.filter(f)
	def take(n: Int)(implicit F: Applicative[F]): Transformer[F, In, In] = Transformer.take(n)
}

class TransformerApplyWithBoundEffect[F[+_]] {
	def op[In, Out](f: In => Emit[Out])(implicit F: Applicative[F]): Transformer[F, In, Out] = Transformer.op(f)
	def map[In, Out](f: In => Out)(implicit F: Applicative[F]): Transformer[F, In, Out] = Transformer.map(f)
	def filter[In](f: In => Boolean)(implicit F: Applicative[F]): Transformer[F, In, In] = Transformer.filter(f)
	def take[In](n: Int)(implicit F: Applicative[F]): Transformer[F, In, In] = Transformer.take(n)
}
