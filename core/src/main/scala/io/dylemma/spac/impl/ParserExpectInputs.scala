package io.dylemma.spac
package impl

import cats.MonadError
import cats.data.NonEmptyList

class ParserExpectInputs[F[+_], In, Out](p: Parser[F, In, Out], expectedInputs: List[(String, In => Boolean)])(implicit F: MonadError[F, Throwable]) extends Parser[F, In, Out] {
	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = expectedInputs match {
		case Nil => p.step(in) // no longer wrapping the parser because all the expected inputs are passed
		case (expectation, predicate) :: nextExpectedInputs =>
			F.flatMap(F.pure(in)) { in =>
				if (predicate(in)) {
					// this input meets our expectation; carry on
					F.map(p.step(in)){ stepResult =>
						stepResult.map(cont => new ParserExpectInputs(cont, nextExpectedInputs))
					}
				} else {
					// input failed our expectations, crash the parser with an error
					F.raiseError {
						new SpacException.UnexpectedInputException(in, NonEmptyList(expectation, nextExpectedInputs.map(_._1)))
					}
				}
			}
	}

	def finish: F[Out] = expectedInputs match {
		case Nil => p.finish // no expectations to enforce, just finish
		case pendingExpectations =>
			F.raiseError {
				new SpacException.UnfulfilledInputsException(NonEmptyList.fromListUnsafe(pendingExpectations.map(_._1)))
			}
	}
}
