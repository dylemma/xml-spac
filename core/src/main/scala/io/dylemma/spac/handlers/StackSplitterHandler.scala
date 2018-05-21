package io.dylemma.spac.handlers

import io.dylemma.spac._
import io.dylemma.spac.types.Stackable

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class StackSplitterHandler[Event, StackElem, Context, P, Out](
	matcher: ContextMatcher[StackElem, Context],
	joiner: Context => HandlerFactory[Event, P],
	val downstream: Handler[P, Out]
)(implicit ctxTracker: ContextTracker[Event, StackElem]) extends SplitterHandlerBase[Event, Context, P, Out] {
	protected def debugName = s"GenericSplitter($matcher){ $joiner } >> $downstream"

	private var currentContext: Option[Try[Context]] = None
	private var matchStartDepth = 0

	/** Return true if a new context was opened and we changed the `currentParserHandler` */
	def maybeOpenNewContext(): Option[Out] = {
		// result may become Some if opening the context fails and we have to pass the error downstream
		var result: Option[Out] = None

		if(currentContext.isEmpty) {
			val newMatch = Try {ctxTracker.checkContext(matcher)} match {
				case Success(ctxOpt) => ctxOpt.map(Success(_))
				case Failure(err) => Some(Failure(err))
			}
			for(tryCtx <- newMatch) {
				currentContext = newMatch
				matchStartDepth = ctxTracker.currentDepth
				tryCtx match {
					case Success(ctx) =>
						currentParserHandler = Some(joiner(ctx).makeHandler())
						debug(s"Entered context: $newMatch at depth $matchStartDepth")
					case Failure(err) =>
						currentParserHandler = None
						result = feedErrorToDownstream(err)
				}
			}
		}

		result
	}

	def maybeCloseCurrentContext(): Option[Out] = {
		if (ctxTracker.currentDepth < matchStartDepth) {
			matchStartDepth = 0
			debug(s"Left context: $currentContext")
			currentContext = None

			// if the context is ending, we need to feed an EOF to any unfinished parser
			feedEndToCurrentParser()
				.map(debug as "Got inner parser result (while closing context)")
				.flatMap(feedResultToDownstream)
		} else {
			None
		}
	}



	// Stack no-ops are a pass-through
	def onContextNoop(event: Event): Option[Out] = {
		feedEventToCurrentParser(event)
			.map(debug as "Got inner parser result (from event)")
			.flatMap(feedResultToDownstream)
	}

	def handleInput(event: Event) = {
		var didPassEvent = false
		var result: Option[Out] = None
		@inline def contextForDebug = s"[${ctxTracker.currentDepth}]${ctxTracker.copyStack.mkString("[ ", " - ", " ]")}"

		def handleMove(move: ContextMove): Unit = if(result.isEmpty) {
			move match {
				case ContextMove.Pass =>
					debug(s"$contextForDebug - event $event")
					didPassEvent = true
					result = onContextNoop(event)

				case ContextMove.Push =>
					debug(s"$contextForDebug - pushed")
					maybeOpenNewContext()

				case ContextMove.Pop =>
					debug(s"$contextForDebug - popped")
					result = maybeCloseCurrentContext()

				case ContextMove.Multi(head, tail) =>
					handleMove(head)
					if(result.isEmpty) handleMove(tail())
			}
		}
		handleMove(ctxTracker.evolve(event))
		if(!didPassEvent && result.isEmpty){
			debug(s"WARN: context tracker didn't provide a Pass instruction for event $event")
			result = onContextNoop(event)
		}
		result
	}
}


