package io.dylemma.spac.handlers

import io.dylemma.spac.types.Stackable
import io.dylemma.spac.{ContextMatcher, Handler, HandlerFactory, Transformer, debug}

import scala.collection.mutable.ArrayBuffer
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

// TODO: resolve similarities between this and the StackSplitterHandler used with Parsers

class SplitterTransformHandler[Event, StackElem, Context, P, Out](
	matcher: ContextMatcher[StackElem, Context],
	joiner: Context => Transformer[Event, P],
	downstream: Handler[P, Out]
)(
	implicit stackable: Stackable.Aux[Event, StackElem]
) //extends SplitterHandlerBase[Event, Context, P, Out] {
	extends Handler[Event, Out]
{
	protected def debugName = s"GenericSplitter($matcher){ $joiner } >> $downstream"
	override def toString = debugName

	def isFinished = downstream.isFinished

	private val stack = new ArrayBuffer[StackElem]
	private var currentContext: Option[Try[Context]] = None
	private var matchStartDepth = 0

	/* while we're in a context, this will be a Some containing a handler for events
	 * in that context. If it finishes, we should None-ify this reference until a
	 * new context begins.
	 */
	private var currentDownstreamWrapper: Option[Handler[Event, Option[Out]]] = None

	/** Send an event/error through the downstream wrapper, possibly causing the downstream to finish.
	  * If the wrapper finishes (due to the transformer it represents becoming finished), we'll clear
	  * the wrapper, but otherwise continue uninterrupted.
	  * @param errorOrEvent
	  * @return `Some` to signify the downstream completed, `None` to continue
	  */
	def feedThroughWrapper(errorOrEvent: Either[Throwable, Event]): Option[Out] = {
		for {
			handler <- currentDownstreamWrapper
			if !handler.isFinished
			downstreamResult <- {
				// Send the event through the wrapper. A `Some` from this implies the downstream finished
				val wrappedResult = errorOrEvent match {
					case Left(err) =>
						try handler.handleError(err) catch { case NonFatal(err) =>
							throw new Exception(s"Error bubbled up through downstream-wrapper to [$handler] while running $debugName", err)
						}
					case Right(event) =>
						try handler.handleInput (event) catch { case NonFatal(err) =>
							throw new Exception(s"Error sending [$event] through downstream-wrapper to [$handler] while running $debugName", err)
						}
				}
				// Since the guard may finish without getting a result from the downstream, check & nullify it here
				if(handler.isFinished){
					currentDownstreamWrapper = None
				}
				// Extract the wrapped result
				wrappedResult match {
					case Some(downstreamResult) => downstreamResult
					case None => None
				}
			}
		} yield {
			// If we're emitting a result, then the wrapper is definitely done, so clear it before returning
			currentDownstreamWrapper = None
			downstreamResult
		}
	}

	def feedContextEndToWrapper(): Option[Out] = {
		for {
			handler <- currentDownstreamWrapper
			if !handler.isFinished
			// triggering an end on the inner transformer may feed an event to the downstream and produce a result
			downstreamResult <- {
				currentDownstreamWrapper = None
				try handler.handleEnd() catch {
					case NonFatal(err) =>
						throw new Exception(s"Error sending [<Context End>] to the inner transformer [$handler] while running $debugName", err)
				}
			}
		} yield {
			currentDownstreamWrapper = None
			downstreamResult
		}
	}

	/** Feed an EOF signal directly to the downstream handler, ignoring the wrapper. */
	def directFeedEndToDownstream() = {
		downstream.handleEnd()
	}

	/** Feed an Error signal directly to the downstream handler, ignoring the wrapper. */
	def directFeedErrorToDownstream(err: Throwable) = {
		downstream.handleError(err)
	}

	/** Create a new handler that forwards inputs and errors to the downstream handler,
	  * but does not forward the End state. We use this handler to handle events within
	  * a context, and the end of the context does not constitute the end of the stream.
	  * To keep the types happy (since `handleEnd` needs to return a value), we wrap the
	  * downstream's output type with `Option`. If the downstream finishes as the result
	  * of an input/error, we'll get a `Some(Some(result))`. An unfinished state will
	  * still be a `None`, as will the result of `handleEnd()`.
	  * @param ctx
	  * @return
	  */
	def createHandlerWrapper(ctx: Context): Handler[Event, Option[Out]] = joiner(ctx).makeHandler(new Handler[P, Option[Out]] {
		override def toString = "DownstreamHandlerWrapper"
		private var handledEnd = false
		def isFinished = handledEnd || downstream.isFinished
		// Send inputs/errors downstream.
		// If the downstream returns `None`, we'll continue by returning `None`.
		// If the downstream returns `Some(r)`, we can finish by returning `Some(Some(r))` that wraps it
		def handleInput(input: P) = downstream.handleInput(input).map(Some(_))
		def handleError(error: Throwable) = downstream.handleError(error).map(Some(_))

		// protect the downstream from the context end, since it's not the end of the whole stream
		def handleEnd() = {
			handledEnd = true
			None
		}
	})

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
						currentDownstreamWrapper = Some(createHandlerWrapper(ctx))
						debug(s"Entered context: $newMatch at depth $matchStartDepth")
					case Failure(err) =>
						currentDownstreamWrapper = None
						debug(s"Failed to enter new context due to error")
						result = directFeedErrorToDownstream(err)
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

			// if the context is ending, pass an EOF to the inner transformer
			feedContextEndToWrapper()
				.map(debug as "Got inner parser result (while closing context)")
		} else {
			None
		}
	}

	// Send an event into the current parser.
	// If that produces a result, send that result to the downstream handler.
	def sendEvent(event: Event): Option[Out] = {
		feedThroughWrapper(Right(event))
			.map(debug as "Got downstream result (from event)")
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

	def handleError(err: Throwable) = {
		feedThroughWrapper(Left(err))
			.map(debug as "Got downstream result (from error)")
	}

	def handleEnd() = {
		directFeedEndToDownstream()
	}
}