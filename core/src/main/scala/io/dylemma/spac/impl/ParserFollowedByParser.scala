package io.dylemma.spac
package impl

class ParserFollowedByParser[In, A, Out](base: Parser[In, A], followUp: A => Parser[In, Out])(implicit stacking: StackLike[In, Any]) extends Parser[In, Out] {
	def newHandler = new ParserFollowedByParser.Handler(base.newHandler, Nil, followUp, stacking)
}

object ParserFollowedByParser {
	class Handler[In, A, Out](
		var base: Parser.Handler[In, A],
		var stackEvents: List[In],
		followUp: A => Parser[In, Out],
		stacking: StackLike[In, Any]
	) extends Parser.Handler[In, Out]
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
					// base parser is still going. Update state and continue
					base = cont
					Right(this)

				case Left(a) =>
					// base parser completed, so now we can create our followUp transformer
					// replay the events that led to our current stack state (head is the latest, so reverse the list)
					followUp(a).newHandler.stepMany(stackEvents.reverse) match {
						case Left((out, leftovers)) =>
							// followUp parser ended as a result of the stackEvents (and we can discard the "leftover inputs")
							Left(out)
						case Right(cont) =>
							// followUp parser is ready to continue
							Right(cont)
					}
			}
		}

		def finish() = {
			// unlike with `step`, `finish` shouldn't change the stack, so we skip right to finishing the parser
			val a = base.finish()

			// initialize the follow-up transformer and feed it whatever is in our stackEvents list
			followUp(a).newHandler.stepMany(stackEvents.reverse) match {
				case Left((out, leftovers)) =>
					// followUp transformer ended as a result of the stackElements (and we can discard the "leftover inputs")
					out
				case Right(cont) =>
					// followUp parser is ready for more... but there is no more so we have to `finish` it
					cont.finish()
			}
		}
	}
}