package io.dylemma.spac
package impl

class ParserFollowedByTransformer[In, A, Out](base: Parser[In, A], followUp: A => Transformer[In, Out])(implicit stacking: StackLike[In, Any]) extends Transformer[In, Out] {
	def newHandler = new ParserFollowedByTransformer.Handler(base, followUp, stacking)
}

object ParserFollowedByTransformer {
	class Handler[In, A, Out](
		parser: Parser[In, A],
		getFollowUp: A => Transformer[In, Out],
		stacking: StackLike[In, Any]
	) extends Transformer.Handler[In, Out]
	{
		private var base = parser.newHandler
		private var followUp: Option[Transformer.Handler[In, Out]] = None
		private var stackEvents: List[In] = Nil

		def push(in: In, out: Transformer.HandlerWrite[Out]): Signal = followUp match {
			case None => pushBeforeFollowUp(in, out)
			case Some(h) => h.push(in, out)
		}

		private def pushBeforeFollowUp(in: In, out: Transformer.HandlerWrite[Out]): Signal =  {
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
					Signal.Continue

				case Left(a) =>
					// base parser completed, so now we can create our followUp transformer
					// replay the events that led to our current stack state (head is the latest, so reverse the list)
					val th = getFollowUp(a).newHandler
					followUp = Some(th)
					th.pushMany(stackEvents.reverseIterator, out)
			}
		}

		def finish(out: Transformer.HandlerWrite[Out]): Unit = followUp match {
			case None =>
				// unlike with `push`, `finish` shouldn't change the stack, so we skip right to finishing the parser
				val a = base.finish()
				// initialize the follow-up transformer and feed it whatever is in our stackEvents list
				val th = getFollowUp(a).newHandler
				val alreadyFinished = th.pushMany(stackEvents.reverseIterator, out).isStop
				// if that didn't cause the handler to "stop", finish it
				if (!alreadyFinished) th.finish(out)

			case Some(h) =>
				// if we already swapped to the follow-up handler, just delegate to it
				h.finish(out)
		}

	}
}