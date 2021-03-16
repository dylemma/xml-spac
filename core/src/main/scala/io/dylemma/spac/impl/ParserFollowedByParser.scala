package io.dylemma.spac
package impl

import cats.Monad
import cats.syntax.all._
import io.dylemma.spac.types.Stackable2

class ParserFollowedByParser[F[+_], In, A, S, Out](
	base: Parser[F, In, A],
	followUp: A => Parser[F, In, Out],
	stackEvents: List[In],
	stacking: Stackable2[In, Any]
)(implicit F: Monad[F]) extends Parser[F, In, Out] {

	def step(in: In): F[Either[Out, Parser[F, In, Out]]] = {
		// first, determine if we need to push or pop a value from the `stackEvents
		val nextStackEvents = stacking.interpretOne(in) match {
			case StackInterpretation.AnyChange(change) =>
				change match {
					case ContextPush(_, _) => in :: stackEvents
					case ContextPop => stackEvents.tail
				}
			case _ => stackEvents
		}

		// second, run the base parser to see if we can transition to the followUp
		base.step(in).flatMap {
			case Right(cont) =>
				// base parser is still going. Emit nothing and continue
				F.pure { Right(new ParserFollowedByParser(cont, followUp, nextStackEvents, stacking)) }

			case Left(a) =>
				// base parser completed, so now we can create our followUp transformer
				// replay the events that led to our current stack state (head is the latest, so reverse the list)
				followUp(a).stepMany(stackEvents.reverse).map {
					case Left((out, leftovers)) =>
						// followUp parser ended as a result of the stackEvents (and we can discard the "leftover inputs")
						Left(out)
					case Right(cont) =>
						// followUp parser is ready to continue
						Right(cont)
				}
		}
	}

	def finish: F[Out] = {
		// unlike with `step`, `finish` shouldn't change the stack, so we skip right to finishing the parser
		base.finish.flatMap { a =>
			// initialize the follow-up transformer and feed it whatever is in our stackEvents list
			followUp(a).stepMany(stackEvents.reverse).flatMap {
				case Left((out, leftovers)) =>
					// followUp transformer ended as a result of the stackElements (and we can discard the "leftover inputs")
					F.pure(out)
				case Right(cont) =>
					// followUp parser is ready for more... but there is no more so we have to `finish` it
					cont.finish
			}
		}
	}
}
