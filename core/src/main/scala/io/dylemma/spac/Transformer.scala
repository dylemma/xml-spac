package io.dylemma.spac

import io.dylemma.spac.impl._
import io.dylemma.spac.types.Unconsable

import scala.annotation.tailrec

trait Transformer[-In, +Out] {
	def newHandler: Transformer.Handler[In, Out]

	def mapBatch[Out2](f: Emit[Out] => Emit[Out2]): Transformer[In, Out2] = new TransformerMapBatch(this, f)
	def map[Out2](f: Out => Out2): Transformer[In, Out2] = mapBatch(_.map(f))
	def filter(predicate: Out => Boolean): Transformer[In, Out] = mapBatch(_.filter(predicate))
	def collect[Out2](pf: PartialFunction[Out, Out2]): Transformer[In, Out2] = mapBatch(_.collect(pf))

	def mergeEither[In2 <: In, Out2](right: Transformer[In2, Out2]): Transformer[In2, Either[Out, Out2]] = new TransformerMergeEither(this, right)

	def upcast[In2 <: In, Out2 >: Out]: Transformer[In2, Out2] = this
	def cast[Out2](implicit ev: Out <:< Out2): Transformer[In, Out2] = this.asInstanceOf[Transformer[In, Out2]]

	def through[Out2](next: Transformer[Out, Out2]): Transformer[In, Out2] = new TransformerThrough(this, next)
	def >>[Out2](next: Transformer[Out, Out2]): Transformer[In, Out2] = through(next)

	/* NOTE: `def into` provided by Transformer.InvariantOps */
	def :>[Out2](parser: Parser[Out, Out2]): Parser[In, Out2] = new TransformerIntoParser(this, parser)
}

class TransformerWordInto[In, A](self: Transformer[In, A]) {
	def apply[Out](parser: Parser[A, Out]): Parser[In, Out] = new TransformerIntoParser(self, parser)
	def list: Parser[In, List[A]] = apply(Parser.toList)
	def map[K, V](implicit ev: A <:< (K, V)) = new TransformerIntoParser(self.cast[(K, V)], Parser.toMap[K, V])
	def first: Parser[In, A] = apply(Parser.first)
	def firstOpt: Parser[In, Option[A]] = apply(Parser.firstOpt)
	def tap(f: A => Unit): Parser[In, Unit] = apply(Parser.tap(f))
}

object Transformer {
	trait Stateless[-In, +Out] extends Transformer[In, Out] with Handler[In, Out] {
		def newHandler: this.type = this
	}
	trait Handler[-In, +Out] {
		def step(in: In): (Emit[Out], Option[Handler[In, Out]])
		def finish(): Emit[Out]

		def stepMany[C[_], In2 <: In](_ins: C[In2])(implicit C: Unconsable[C]): (Emit[Out], Either[C[In2], Handler[In, Out]]) = {
			// fold inputs from `_ins` into this transformer, accumulating a buffer of `outs` and updating the transformer state along the way,
			// eventually returning the concatenation of all `outs`, and the final `transformer` state if the transformer is ready to continue,
			// or else the leftover unconsumed inputs if the transformer decided to end partway through
			var out: Emit[Out] = Emit.nil
			@tailrec def loop(current: Handler[In, Out], remaining: C[In2]): Either[C[In2], Handler[In, Out]] = {
				C.uncons(remaining) match {
					case Some((in, tail)) =>
						// next input, step the transformer
						val (emit, nextState) = current.step(in)
						out ++= emit
						nextState match {
							case None => Left(tail)
							case Some(cont) => loop(cont, tail)
						}
					case None =>
						// end of input, exit recursion by returning a Right with the current transformer
						Right(current)
				}
			}
			val endState = loop(this, _ins)
			out -> endState
		}
	}

	def apply[In] = new TransformerApplyBound[In]

	def identity[In]: Transformer[In, In] = new TransformerIdentity
	def op[In, Out](f: In => Emit[Out]): Transformer[In, Out] = new TransformerOp(f)
	def map[In, Out](f: In => Out): Transformer[In, Out] = op { in => Emit.one(f(in)) }
	def filter[In](f: In => Boolean): Transformer[In, In] = op { in => if (f(in)) Emit.one(in) else Emit.empty }
	def take[In](n: Int): Transformer[In, In] = new TransformerTake(n)
	def takeWhile[In](f: In => Boolean): Transformer[In, In] = new TransformerTakeWhile(f)

	implicit class TransformerInvariantOps[In, Out](val self: Transformer[In, Out]) extends AnyVal {
		def into = new TransformerWordInto[In, Out](self)
	}
}

class TransformerApplyBound[In] {
	def identity: Transformer[In, In] = new TransformerIdentity
	def op[Out](f: In => Emit[Out]): Transformer[In, Out] = new TransformerOp(f)
	def map[Out](f: In => Out): Transformer[In, Out] = op { in => Emit.one(f(in)) }
	def filter(f: In => Boolean): Transformer[In, In] = op { in => if (f(in)) Emit.one(in) else Emit.empty }
	def take(n: Int): Transformer[In, In] = new TransformerTake(n)
	def takeWhile(f: In => Boolean): Transformer[In, In] = new TransformerTakeWhile(f)
}
