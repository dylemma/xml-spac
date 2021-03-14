package io.dylemma.spac
package impl

import cats.MonadError
import cats.data.NonEmptyList
import cats.implicits._
import io.dylemma.spac.impl.ParserOrElseList.NoSuccessfulParsersException

object ParserOrElseList {
	class NoSuccessfulParsersException(errors: List[Throwable]) extends Exception("All of the parsers in the orElse chain failed") {
		for (err <- errors) this.addSuppressed(err)
	}
}

case class ParserOrElseList[F[+_], In, Out](state: List[Either[Throwable, Parser[F, In, Out]]])(implicit F: MonadError[F, Throwable]) extends Parser[F, In, Out] {

	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = {
		F.tailRecM(RecState(state, Nil))(_.stepForTailRecM(in)).map(_.map(new ParserOrElseList(_)))
	}
	def finish: F[Out] = {
		F.tailRecM(RecState(state, Nil))(_.finishForTailRecM)
	}

	override def orElse[In2 <: In, Out2 >: Out](fallback: Parser[F, In2, Out2])(implicit F: MonadError[F, Throwable]): Parser[F, In2, Out2] = fallback match {
		case ParserOrElseList(fallbackState) => ParserOrElseList(state ++ fallbackState)
		case regularFallback => ParserOrElseList(state :+ Right(regularFallback))
	}

	/** The way ParserOrElseList is intended to operate is that for each `step` or `finish` call,
	  * we search through the `state` list for a parser that will yield a result.
	  *
	  * Assuming no parser yields any result, it means they are either still running, or at least
	  * one of them has failed with an exception.
	  *
	  * If we reach a point where *all* of the parsers have failed with an exception, then
	  * we can take the exception at the end of the "orElse" chain and raise it as an error.
	  *
	  * We do all of this via a recursive function that consumes the `state` list and
	  * builds the next version of that `state` along the way.
	  *
	  * The methods in this inner class are tailored around the use of `F.tailRecM`,
	  * such that they return either the next state for the recursion, or a value that
	  * the parser's own `step` or `finish` method should then return.
	  *
	  * @param pending
	  * @param built
	  */
	private case class RecState(
		pending: List[Either[Throwable, Parser[F, In, Out]]],
		built: List[Either[Throwable, Parser[F, In, Out]]]
	) {
		def stepForTailRecM(in: In) = pending match {
			case Left(err) :: pendingTail => Left(RecState(pendingTail, Left(err) :: built)).pure[F]
			case Right(cont) :: pendingTail => F.attempt(cont step in) map {
				case Left(err) => Left(RecState(pendingTail, Left(err) :: built))
				case Right(Left(out)) =>
					Right(Left(out)) // end recursion with a result!
				case Right(Right(cont2)) =>
					Left(RecState(pendingTail, Right(cont2) :: built))
			}
			case Nil =>
				// End recursion with the next "state"
				// Since we built up the state by prepending, we have to reverse it to put it back in the proper order
				if (built.forall(_.isLeft)) {
					// all of the parsers have failed. The one at the head of the 'built' list should be furthest back in the chain,
					// so we'll grab that one and add all the others as "suppressed" exceptions
					if (built.isEmpty) {
						F.raiseError(new IllegalStateException("ParserOrElseList step with no inner parsers"))
					} else {
						val errors = built.collect { case Left(ex) => ex }
						F.raiseError { new NoSuccessfulParsersException(errors) }
					}
				} else {
					Right(Right(built.reverse)).pure[F]
				}
		}

		def finishForTailRecM = pending match {
			case Left(err) :: pendingTail => Left(RecState(pendingTail, Left(err) :: built)).pure[F]
			case Right(cont) :: pendingTail => F.attempt(cont.finish) map {
				case Left(err) => Left(RecState(pendingTail, Left(err) :: built))
				case Right(out) => Right(out) // end recursion with a result!
			}
			case Nil =>
				if (built.isEmpty) {
					F.raiseError(new IllegalStateException("ParserOrElseList finish with no inner parsers"))
				} else {
					// we can assume `built` is full of `Left(error)` since if any of the parsers got a successful result,
					// we would have exited recursion and never made it to this point
					val errors = built.collect { case Left(ex) => ex }
					F.raiseError { new NoSuccessfulParsersException(errors) }
				}
		}
	}
}
