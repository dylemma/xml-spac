package io.dylemma.spac.handlers

import io.dylemma.spac._
import io.dylemma.spac.core.{ContextMove, Format}

import scala.util.{Failure, Success, Try}

class StackFormatSplitterHandler[Event, StackElem, Context, P, Out](
	contextual: Format.Aux[Event, StackElem],
	matcher: ContextMatcher[StackElem, Context],
	joiner: Context => HandlerFactory[Event, P],
	val downstream: Handler[P, Out]
) extends SplitterHandlerBase[Event, Context, P, Out] {
	protected def debugName = s"GenericSplitter($matcher){ $joiner } >> $downstream"
	private val ctxTracker = contextual.makeContextTracker

	private var currentContext: Option[Try[Context]] = None
	private var matchStartDepth = 0

	def handleInput(event: Event) = {
		debug(s"event: $event")
		ctxTracker.evolve(event) match {

			// Pushing to the stack may open a new context
			case ContextMove.Push =>
				// attempt to match a context if there isn't one already
				if(currentContext.isEmpty){
					val newMatch = Try { ctxTracker.checkContext(matcher) } match {
						case Success(ctxOpt) => ctxOpt.map(Success(_))
						case Failure(err) => Some(Failure(err))
					}
					for(tryCtx <- newMatch){
						currentContext = newMatch
						matchStartDepth = ctxTracker.currentDepth
						tryCtx match {
							case Success(ctx) =>
								currentParserHandler = Some(joiner(ctx).makeHandler())
								debug(s"Entered context: $newMatch")
							case Failure(err) =>
								currentParserHandler = None
								feedErrorToDownstream(err)
						}
					}
				}

				// feed the event to the current parser, if there is one
				feedEventToCurrentParser(event)
					.map(debug as "Got inner parser result (from start element)")
					.flatMap(feedResultToDownstream)

			// Popping from the stack may close the current context
			case ContextMove.Pop =>
				// feed the event to the current parser, if there is one
				val downstreamOutput = feedEventToCurrentParser(event)
					.map(debug as "Got inner parser result (from context end)")
					.flatMap(feedResultToDownstream)

				// as long as that last event didn't cause the parser to end,
				// we still need to feed an EOF to the parser
				downstreamOutput orElse {
					// end the context if the stack goes below the place where the current context opened
					if (ctxTracker.currentDepth < matchStartDepth) {
						matchStartDepth = 0
						debug(s"Left context: $currentContext")
						currentContext = None

						// if the context is ending, we need to feed an EOF to any unfinished parser
						feedEndToCurrentParser()
							.map(debug as "Got inner parser result (from post context end)")
							.flatMap(feedResultToDownstream)
					} else {
						None
					}
				}

			// Stack no-ops are a pass-through
			case ContextMove.Noop =>
				feedEventToCurrentParser(event)
					.map(debug as "Got inner parser result (from event)")
					.flatMap(feedResultToDownstream)
		}
	}
}


