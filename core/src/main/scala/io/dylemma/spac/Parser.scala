package io.dylemma.spac

import cats.data.Chain
import cats.effect.{Bracket, SyncIO}
import cats.{Applicative, Monad}
import io.dylemma.spac.impl._
import io.dylemma.spac.types.Unconsable
import org.tpolecat.typename.TypeName

import scala.annotation.tailrec
import scala.util.Try

trait Parser[-In, +Out] { self =>

	def newHandler: Parser.Handler[In, Out]

//	/** Advance the state of this parser by accepting a single input of type `In`.
//	  * If doing so would cause this parser to complete, return a `Left` containing the output.
//	  * Otherwise, return a `Right` containing the next parser state.
//	  *
//	  * Note that while the return type of this method is wrapped in the `F` effect type,
//	  * the actual work of this method need not necessarily happen as an "effect" within the F context.
//	  * It is acceptable to perform computations and then simply return `F.pure(result)`.
//	  * The expectation is that this method will only be called while already inside the F context,
//	  * e.g. via `stepMany`, which uses `F.tailRecM`, or via the `parse` methods, which also use `F.tailRecM`.
//	  * This is a slight concession in order to discourage Parser implementations from doing things like
//	  * `F.pure(in).map { /* actual work here */ }`
//	  *
//	  * @param in
//	  * @return either the final result of the parser, or the parser's next state
//	  */
//	def step(in: In): Either[Out, Parser[In, Out]]


//	/** Finish this parser by accepting an "end of stream" signal.
//	  * All parsers *must* produce a value of type `Out` with this method, or else raise an error in the F context.
//	  *
//	  * As with `step`, the actual work of this method need not necessarily happen within the F context.
//	  *
//	  * @return the final result of this parser
//	  */
//	def finish(): Out

//	/** Make a copy of this Parser with its current state.
//	  * For "pure" parsers, it's acceptable to return `this`.
//	  * For "impure" parsers, any internally-mutable state, including references to other parsers, should be cloned to construct a new Parser of the same type.
//	  */
//	def cloneState: Parser[In, Out]
	def withName(name: String): Parser[In, Out] = new ParserNamed(this, name)

	def map[Out2](f: Out => Out2): Parser[In, Out2] = new ParserMapped(this, f)

	def orElse[In2 <: In, Out2 >: Out](fallback: Parser[In2, Out2]): Parser[In2, Out2] = ParserOrElseChain[In2, Out2](Chain(this, fallback))

	def wrapSafe: Parser[In, Try[Out]] = new ParserTry(this)
	def attempt: Parser[In, Either[Throwable, Out]] = wrapSafe.map(_.toEither)

	def expectInputs[I2 <: In](expectations: List[(String, I2 => Boolean)]): Parser[I2, Out] = new ParserExpectInputs(this, expectations)

	def interruptedBy[I2 <: In](interrupter: Parser[I2, Any]): Parser[I2, Out] = new ParserInterruptedBy(this, interrupter)
	def beforeContext[I2 <: In, StackElem](matcher: ContextMatcher[StackElem, Any])(implicit stackable: StackLike[I2, StackElem]): Parser[I2, Out] = {
		// use ContextMatchSplitter to drive the stackable+matcher together, and pipe it into a parser that returns when a ContextPush is interpreted,
		// i.e. the `interrupter` will yield a result upon entering a context matched by the `matcher`
		interruptedBy { Splitter[I2].fromMatcher(matcher).addBoundaries.collect { case Left(ContextPush(_, _)) => () } :> Parser.firstOpt }
	}
	def upcast[Out2](implicit ev: Out <:< Out2): Parser[In, Out2] = this.asInstanceOf[Parser[In, Out2]]

	def asTransformer: Transformer[In, Out] = new ParserAsTransformer(this)

	def parseSeq[C[_], In2 <: In](source: C[In2])(implicit srcAsSeq: Unconsable[C]): Out = {
		newHandler.stepMany(source) match {
			case Left((out, _)) => out
			case Right(cont) => cont.finish()
		}
	}

	def parse[Src](source: Src)(implicit srcToPullable: ToPullable[SyncIO, Src, In]): Out = {
		parseF[SyncIO, Src](source).unsafeRunSync()
	}

	def parseF[F[+_], Src](source: Src)(implicit srcToPullable: ToPullable[F, Src, In], F: Monad[F], Fb: Bracket[F, Throwable]): F[Out] = {
		srcToPullable(source).use[F[+*], Out] { srcPullable =>
			F.tailRecM(srcPullable -> newHandler) { case (inputs, handler) =>
				F.map(inputs.uncons) {
					case Some((in, nextInputs)) =>
						handler.step(in) match {
							case Left(result) => Right(result)
							case Right(cont) => Left(nextInputs -> cont)
						}
					case None =>
						Right(handler.finish())
				}
			}
		}
	}
}

class ParserApplyWithBoundInput[In] {
	def app: Applicative[Parser[In, *]] = Parser.parserApplicative

	def firstOpt: Parser[In, Option[In]] = Parser.firstOpt
	def first(implicit In: TypeName[In]): Parser[In, In] = Parser.first
	def find(predicate: In => Boolean): Parser[In, Option[In]] = Parser.find(predicate)
	def fold[Out](init: Out)(op: (Out, In) => Out): Parser[In, Out] = Parser.fold(init)(op)
	def pure[Out](value: Out): Parser[In, Out] = Parser.pure(value)
	def delay[Out](value: => Out): Parser[In, Out] = Parser.delay(value)
	def defer[Out](p: => Parser[In, Out]): Parser[In, Out] = Parser.defer(p)
	def deferHandler[Out](h: => Parser.Handler[In, Out]): Parser[In, Out] = Parser.deferHandler(h)
	def fromBuilder[Out](b: => collection.mutable.Builder[In, Out]): Parser[In, Out] = Parser.fromBuilder(b)
	def toList: Parser[In, List[In]] = Parser.toList
	def toChain: Parser[In, Chain[In]] = Parser.toChain
	def toMap[K, V](implicit ev: In <:< (K, V)): Parser[(K, V), Map[K, V]] = Parser.toMap[K, V]
	def tap(f: In => Unit): Parser[In, Unit] = Parser.tap(f)
	def drain: Parser[In, Unit] = Parser.drain
}

object Parser {
	trait Stateless[-In, +Out] extends Parser[In, Out] with Handler[In, Out] {
		def newHandler: this.type = this
	}
	trait Handler[-In, +Out] {
		def step(in: In): Either[Out, Handler[In, Out]]
		def finish(): Out

		def stepMany[C[_], In2 <: In](inputs: C[In2])(implicit C: Unconsable[C]): Either[(Out, C[In2]), Handler[In, Out]] = {
			@tailrec def loop(current: Handler[In, Out], remaining: C[In2]): Either[(Out, C[In2]), Handler[In, Out]] = {
				C.uncons(remaining) match {
					case Some((head, tail)) =>
						current.step(head) match {
							case Right(cont) => loop(cont, tail)
							case Left(out) => Left(out -> tail)
						}
					case None =>
						Right(current)
				}
			}
			loop(this, inputs)
		}
	}


	def apply[In] = new ParserApplyWithBoundInput[In]

	def firstOpt[In]: Parser[In, Option[In]] = new ParserFirstOpt[In]
	def first[In: TypeName]: Parser[In, In] = new ParserFirst[In]
	def find[In](predicate: In => Boolean): Parser[In, Option[In]] = new ParserFind(predicate)
	def fold[In, Out](init: Out)(op: (Out, In) => Out): Parser[In, Out] = new ParserFold(init, op)
	def pure[Out](value: Out): Parser[Any, Out] = new ParserPure(value)
	def delay[Out](value: => Out): Parser[Any, Out] = new ParserDelay(() => value)
	def defer[In, Out](p: => Parser[In, Out]): Parser[In, Out] = new ParserDefer(() => p)
	def deferHandler[In, Out](h: => Parser.Handler[In, Out]): Parser[In, Out] = new ParserDeferHandler(() => h)
	def fromBuilder[In, Out](b: => collection.mutable.Builder[In, Out]): Parser[In, Out] = deferHandler { new ParserHandlerForBuilder(b) }
	def toList[In]: Parser[In, List[In]] = fromBuilder { List.newBuilder }
	def toChain[In]: Parser[In, Chain[In]] = fold(Chain.empty[In])(_ :+ _)
	def toMap[K, V]: Parser[(K, V), Map[K, V]] = fromBuilder { Map.newBuilder[K, V] }
	def tap[In](f: In => Unit): Parser[In, Unit] = new ParserTap(f)
	def drain: Parser[Any, Unit] = ParserDrain

	@deprecated("use `.pure` instead", "v0.9")
	def constant[Out](value: Out) = pure(value)

	implicit class TryParserOps[-In, +Out](parser: Parser[In, Try[Out]]) {
		def unwrapSafe: Parser[In, Out] = new ParserRethrow(parser)
	}
	implicit class ParserFollowedByOps[In, A](parser: Parser[In, A]) {
		def followedByParser = followedBy
		def followedBy: FollowedBy[In, A, Parser] = new FollowedBy[In, A, Parser] {
			def apply[Out](followUp: A => Parser[In, Out])(implicit S: StackLike[In, Any]): Parser[In, Out] = {
				new ParserFollowedByParser(parser, followUp)
			}
		}
		def followedByStream: FollowedBy[In, A, Transformer] = new FollowedBy[In, A, Transformer] {
			def apply[Out](followUp: A => Transformer[In, Out])(implicit S: StackLike[In, Any]): Transformer[In, Out] = {
				new ParserFollowedByTransformer(parser, followUp)
			}
		}
	}

	implicit def parserApplicative[In]: Applicative[Parser[In, *]] = new Applicative[Parser[In, *]] {
		def pure[A](x: A) = new ParserPure(x)
		def ap[A, B](ff: Parser[In, A => B])(fa: Parser[In, A]) = product(fa, ff).map { case (a, f) => f(a) }
		override def product[A, B](fa: Parser[In, A], fb: Parser[In, B]) = {
			(fa, fb) match {
				case (faCompound: ParserCompoundN[In, A], fbCompound: ParserCompoundN[In, B]) =>
					val offset = faCompound.members.size.toInt
					new ParserCompoundN(
						faCompound.members ++ fbCompound.members,
						get => faCompound.assemble(get) -> fbCompound.assemble(OffsetGet(offset, get))
					)
//					fbCompound.compoundProductWithLhs(faCompound)
				case (fa, fbCompound: ParserCompoundN[In, B]) =>
					new ParserCompoundN(
						fa +: fbCompound.members,
						get => get(0).asInstanceOf[A] -> fbCompound.assemble(OffsetGet(1, get))
					)
//					fbCompound.productWithLhs(fa)
				case (faCompound: ParserCompoundN[In, A], fb) =>
					val offset = faCompound.members.size.toInt
					new ParserCompoundN(
						faCompound.members :+ fb,
						get => faCompound.assemble(get) -> get(offset).asInstanceOf[B]
					)
//					faCompound.productWithRhs(fb)
				case (fa, fb) =>
					new ParserCompoundN(
						Chain(fa, fb),
						results => (results(0).asInstanceOf[A], results(1).asInstanceOf[B])
					)
			}
		}
		override def map[A, B](fa: Parser[In, A])(f: A => B): Parser[In, B] = fa.map(f)

		private case class OffsetGet private(offset: Int, get: Int => Any) extends (Int => Any) {
			def apply(i: Int) = get(i + offset)
		}
		private object OffsetGet {
			def apply(offset: Int, get: Int => Any) = get match {
				case OffsetGet(n, g) => new OffsetGet(n + offset, g)
				case g => new OffsetGet(offset, g)
			}
		}

	}

//	class ParserWordFollowed[In, +A](self: Parser[In, A]) {
//		def by: FollowedBy[In, A, Parser[In, +*]] = new FollowedBy[In, A, Parser[In, +*]] {
//			def apply[Out](followUp: A => Parser[In, Out])(implicit S: StackLike[In, Any]): Parser[In, Out] = {
//				new ParserFollowedByParser(self, followUp)
//			}
//		}
//		def byStream: FollowedBy[In, A, Transformer[In, +*]] = new FollowedBy[In, A, Transformer[In, +*]] {
//			def apply[Out](followUp: A => Transformer[In, Out])(implicit S: StackLike[In, Any]): Transformer[In, Out] = {
//				new ParserFollowedByTransformer(self, followUp)
//			}
//		}
//	}

	trait FollowedBy[In, +A, M[-_, +_]] { self =>
		def apply[Out](followUp: A => M[In, Out])(implicit S: StackLike[In, Any]): M[In, Out]

		def flatMap[Out](followUp: A => M[In, Out])(implicit S: StackLike[In, Any]): M[In, Out] = apply(followUp)

		def map[B](f: A => B)(implicit S: StackLike[In, Any]): FollowedBy[In, B, M] = new FollowedBy[In, B, M] {
			def apply[Out](followUp: B => M[In, Out])(implicit S: StackLike[In, Any]) = self { a => followUp(f(a)) }
		}
	}
}