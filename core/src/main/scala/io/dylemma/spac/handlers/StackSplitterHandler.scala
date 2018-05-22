package io.dylemma.spac.handlers

import io.dylemma.spac._
import io.dylemma.spac.types.Stackable

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

class StackSplitterHandler[Event, StackElem, Context, P, Out](
	matcher: ContextMatcher[StackElem, Context],
	joiner: Context => HandlerFactory[Event, P],
	val downstream: Handler[P, Out]
)(implicit stackable: Stackable.Aux[Event, StackElem]) extends SplitterHandlerBase[Event, Context, P, Out] {
	protected def debugName = s"GenericSplitter($matcher){ $joiner } >> $downstream"

	private val stack = new ArrayBuffer[StackElem]
	private var currentContext: Option[Try[Context]] = None
	private var matchStartDepth = 0

	// Push to the stack.
	// Doing so may cause a new context to start.
	// If context matching fails, the error will be forwarded downstream,
	// possibly producing a result that would end this handler.
	def performStackPush(newElem: StackElem): Option[Out] = {
		// push the element to the stack, then see if that opens a new context
		stack += newElem

		// result may become Some if opening the context fails and we have to pass the error downstream
		var result: Option[Out] = None

		if (currentContext.isEmpty) {
			val newMatch = Try {matcher(stack, 0, stack.size)} match {
				case Success(ctxOpt) => ctxOpt.map(Success(_))
				case Failure(err) => Some(Failure(err))
			}
			for (tryCtx <- newMatch) {
				currentContext = newMatch
				matchStartDepth = stack.size
				tryCtx match {
					case Success(ctx) =>
						currentParserHandler = Some(joiner(ctx).makeHandler())
						debug(s"Entered context: $newMatch at depth $matchStartDepth")
					case Failure(err) =>
						currentParserHandler = None
						debug(s"Failed to enter new context due to error")
						result = feedErrorToDownstream(err)
				}
			}
		}

		result
	}

	// Pop from the stack.
	// Doing so may end the current context, so we have to send EOF signals
	// and handle any results generated from doing so.
	def performStackPop(): Option[Out] = {
		stack.remove(stack.size - 1)

		if (stack.size < matchStartDepth) {
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

	// Send an event into the current parser.
	// If that produces a result, send that result to the downstream handler.
	def sendEvent(event: Event): Option[Out] = {
		feedEventToCurrentParser(event)
			.map(debug as "Got inner parser result (from event)")
			.flatMap(feedResultToDownstream)
	}

	def handleInput(event: Event) = {
		stackable.asPush(event) match {
			// PUSH
			case Some(elem) =>
				// Push and Send, but the order depends on the Stackable
				if (stackable.order(event) >= 0) {
					performStackPush(elem) orElse sendEvent(event)
				} else {
					sendEvent(event) orElse performStackPush(elem)
				}

			// POP
			case None if stackable.isPop(event) =>
				// Pop and Send, but the order depends on the Stackable
				if (stackable.order(event) >= 0) {
					performStackPop() orElse sendEvent(event)
				} else {
					sendEvent(event) orElse performStackPop()
				}

			// NO CHANGE
			case _ =>
				sendEvent(event)
		}
	}
}


