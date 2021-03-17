package io.dylemma.spac
package impl

import cats.Monad
import cats.syntax.all._

class ParserFollowedByTransformer[F[+_], In, A, S, Out](
	base: Parser[F, In, A],
	followUp: A => Transformer[F, In, Out],
	stackEvents: List[In],
	stacking: StackLike[In, Any]
)(implicit F: Monad[F]) extends Transformer[F, In, Out] {
	def step(in: In): F[(Emit[Out], Option[Transformer[F, In, Out]])] = {
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
				F.pure { Emit.nil -> Some(new ParserFollowedByTransformer(cont, followUp, nextStackEvents, stacking)) }

			case Left(a) =>
				// base parser completed, so now we can create our followUp transformer
				// replay the events that led to our current stack state (head is the latest, so reverse the list)
				followUp(a).stepMany(stackEvents.reverse).map {
					// followUp transformer ended as a result of the stackEvents (and we can discard the "leftover inputs")
					case (toEmit, Left(_)) => toEmit -> None
					// followUp transformer is ready to continue
					case (toEmit, Right(cont)) => toEmit -> Some(cont)
				}
		}
	}

	def finish: F[Emit[Out]] = {
		// unlike with `step`, `finish` shouldn't change the stack, so we skip right to finishing the parser
		base.finish.flatMap { a =>
			// initialize the follow-up transformer and feed it whatever is in our stackEvents list
			followUp(a).stepMany(stackEvents.reverse).flatMap {
				case (toEmit, Left(_)) =>
					// followUp transformer ended as a result of the stackElements (and we can discard the "leftover inputs")
					F.pure(toEmit)
				case (toEmit, Right(cont)) =>
					// followUp transformer is ready for more... but there is no more so we have to `finish` it
					cont.finish.map { toEmit ++ _ }
			}
		}
	}
}
