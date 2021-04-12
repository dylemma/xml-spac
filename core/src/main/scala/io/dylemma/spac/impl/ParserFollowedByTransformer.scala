package io.dylemma.spac
package impl

class ParserFollowedByTransformer[In, A, Out](base: Parser[In, A], followUp: A => Transformer[In, Out])(implicit stacking: StackLike[In, Any]) extends Transformer[In, Out] {
	def newHandler = new ParserFollowedByTransformer.Handler(base.newHandler, Nil, followUp, stacking)
}

object ParserFollowedByTransformer {
	class Handler[In, A, Out](
		private var base: Parser.Handler[In, A],
		private var stackEvents: List[In],
		followUp: A => Transformer[In, Out],
		stacking: StackLike[In, Any]
	) extends Transformer.Handler[In, Out]
	{

		def step(in: In) = {
			// update the `stackEvents` list if the current input counts as a push or a pop
			stackEvents = stacking.interpretOne(in) match {
				case StackInterpretation.AnyChange(change) =>
					change match {
						case ContextPush(_, _) => in :: stackEvents
						case ContextPop => stackEvents.tail
					}
				case _ => stackEvents
			}

			// second, run the base parser to see if we can transition to the followUp
			base.step(in) match {
				case Right(cont) =>
					// base parser is still going. Update and continue
					base = cont
					Emit.nil -> Some(this)

				case Left(a) =>
					// base parser completed, so now we can create our followUp transformer
					// replay the events that led to our current stack state (head is the latest, so reverse the list)
					followUp(a).newHandler.stepMany(stackEvents.reverse) match {
						// followUp transformer ended as a result of the stackEvents (and we can discard the "leftover inputs")
						case (toEmit, Left(_)) => toEmit -> None
						// followUp transformer is ready to continue
						case (toEmit, Right(cont)) => toEmit -> Some(cont)
					}
			}
		}

		def finish() = {
			// unlike with `step`, `finish` shouldn't change the stack, so we skip right to finishing the parser
			val a = base.finish()

			// initialize the follow-up transformer and feed it whatever is in our stackEvents list
			followUp(a).newHandler.stepMany(stackEvents.reverse) match {
				case (toEmit, Left(_)) =>
					// followUp transformer ended as a result of the stackElements (and we can discard the "leftover inputs")
					toEmit
				case (toEmit, Right(cont)) =>
					// followUp transformer is ready for more... but there is no more so we have to `finish` it
					Emit.concat(toEmit, cont.finish())
			}
		}
	}
}