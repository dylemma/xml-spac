package io.dylemma.spac

import cats.data.{Chain, NonEmptyChain}
import cats.effect.Resource
import cats.implicits._
import cats.{Applicative, Functor, Monad}
import io.dylemma.spac.types.Unconsable

trait ToPullable[F[+_], S, +A] {
	def apply(source: S): Resource[F, Pullable[F, A]]
}
object ToPullable {
	implicit def forUnconsable[F[+_]: Applicative, C[_]: Unconsable, A]: ToPullable[F, C[A], A] = new ToPullable[F, C[A], A] {
		def apply(source: C[A]) = Resource.liftF {
			source.pure[F].map { Pullable.fromUnconsable[F, C, A] }
		}
	}
}

trait Pullable[F[+_], +A] {
	def uncons: F[Option[(A, Pullable[F, A])]]

	def into[B](parser: Parser[F, A, B])(implicit F: Monad[F]): F[B] = F.tailRecM(this -> parser) { case (currentPull, currentParser) =>
		currentPull.uncons.flatMap {
			case None => currentParser.finish.map(Right(_))
			case Some((a, nextPull)) => currentParser.step(a).flatMap {
				case Left(result) => F.pure(Right(result))
				case Right(nextParser) => F.pure(Left(nextPull -> nextParser))
			}
		}
	}

	def through[B](transformer: Transformer[F, A, B])(implicit F: Monad[F]): Pullable[F, B] = {
		new TransformedPullable(this, transformer)
	}
}
object Pullable {
	def apply[F[+_], A] = new PullablePartialApply[F, A]
	class PullablePartialApply[F[+_], A] {
		def from[S](source: S)(implicit toPullable: ToPullable[F, S, A]): Resource[F, Pullable[F, A]] = toPullable(source)
	}

	def nil[F[+_] : Applicative]: Pullable[F, Nothing] = new Pullable[F, Nothing] {
		def uncons = None.pure[F]
	}

	def cons[F[+_]: Applicative, A](toEmit: Chain[A], next: Pullable[F, A]): Pullable[F, A] = new Pullable[F, A] {
		def uncons = unconsFrom(toEmit, next)
	}
	def unconsFrom[F[+_]: Applicative, A](pendingEmit: Chain[A], next: Pullable[F, A]): F[Option[(A, Pullable[F, A])]] = pendingEmit.uncons match {
		case None => next.uncons
		case Some((head, tail)) =>
			val nextPull = NonEmptyChain.fromChain(tail) match {
				case None => next
				case Some(tailBuffer) => new BufferedPullable[F, A](tailBuffer, next)
			}
			Some(head -> nextPull).pure[F]
	}

	def fromUnconsable[F[+_]: Applicative, C[_]: Unconsable, A](c: C[A]): Pullable[F, A] = {
		new UnconsableWrapper(c.pure[F])
	}
	private class UnconsableWrapper[F[+_], C[_], A](seqF: F[C[A]])(implicit F: Applicative[F], C: Unconsable[C]) extends Pullable[F, A] {
		def uncons: F[Option[(A, UnconsableWrapper[F, C, A])]] = F.map(seqF){ seq =>
			C.uncons(seq) map { case (head, tail) =>
				head -> new UnconsableWrapper(tail.pure[F])
			}
		}
	}
}


private class TransformedPullable[F[+_] : Monad, A, B](base: Pullable[F, A], transformer: Transformer[F, A, B]) extends Pullable[F, B] {
	def uncons = base.uncons.flatMap {
		case Some((head, nextPull)) =>
			transformer.step(head).flatMap {
				// if the transformer ends, we can discard the `nextPull` and end after emitting the `toEmit` buffer
				case (toEmit, None) => Pullable.unconsFrom(toEmit, Pullable.nil[F])
				// otherwise, emit the `toEmit` buffer before continuing on with the 'next' states
				case (toEmit, Some(nextTransformer)) => Pullable.unconsFrom(toEmit, new TransformedPullable(nextPull, nextTransformer))
			}
		case None =>
			// EOF - finish the transformer and emit whatever leftovers it produced
			transformer.finish.flatMap { Pullable.unconsFrom(_, Pullable.nil[F]) }
	}
}
private class BufferedPullable[F[+_] : Applicative, A](buffer: NonEmptyChain[A], next: Pullable[F, A]) extends Pullable[F, A] {
	def uncons = Some((
		buffer.head,
		NonEmptyChain.fromChain(buffer.tail) match {
			case None => next
			case Some(tail) => new BufferedPullable(tail, next)
		}
	)).pure[F]
}