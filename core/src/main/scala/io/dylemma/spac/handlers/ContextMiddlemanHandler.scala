package io.dylemma.spac.handlers

import io.dylemma.spac.{ContextSensitiveHandler, Handler, debug}

import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

/** A context-sensitive Handler creates a "middleman" handler as a proxy to the downstream handler, for each new context that is entered.
  *
  * This handler expects that contexts will not be nested (i.e. `handleContextStart` will not be called twice without a `handleContextEnd`
  * between them). Each time a new context is entered, the middleman creates a wrapper around the downstream handler. All of the usual
  * handler methods are sent through the middleman's wrapper of the downstream handler.
  *
  * @param middleman An object responsible for setting up a wrapper around the `downstream` handler for each new context.
  * @param downstream A handler that consumes the values from the middleman via a wrapper handler
  * @tparam In  The input type
  * @tparam Context The context type
  * @tparam P An intermediate type sent between the middleman and the downstream handler
  * @tparam Out The output type
  */
class ContextMiddlemanHandler[In, Context, P, Out](
	middleman: ContextMiddleman[Context, In, P],
	downstream: Handler[P, Out]
) extends ContextSensitiveHandler[In, Context, Out] {
	protected def debugName = s"$middleman >> downstream"

	/* while we're in a context, this will be a Some containing a handler for events
	 * in that context. If it finishes, we should None-ify this reference until a
	 * new context begins.
	 */
	private var middlemanHandler: Option[Handler[In, Option[Out]]] = None

	def isFinished = downstream.isFinished

	def handleInput(input: In) = feedMiddleman(Right(input))
	def handleError(error: Throwable) = feedMiddleman(Left(error))
	def handleEnd() = feedMiddlemanEOF() getOrElse downstream.handleEnd()

	def handleContextStart(context: Try[Context]) = context match {
		case Success(ctx) =>
			middlemanHandler = Some(middleman.createWrapper(ctx, downstream))
			None
		case Failure(err) =>
			middlemanHandler = None
			debug(s"Failed to enter new context due to error")
			downstream.handleError(err)
	}

	def handleContextEnd() = {
		// if the context is ending, pass an EOF to the inner transformer
		feedMiddlemanEOF()
			.map(debug as "Got inner parser result (while closing context)")
	}

	/** Send an event/error through the middleman, possibly causing the downstream to finish.
	  * If the middleman finishes (due to the transformer it represents becoming finished), we'll clear
	  * the wrapper, but otherwise continue uninterrupted.
	  * @param errorOrEvent
	  * @return `Some` to signify the downstream completed, `None` to continue
	  */
	protected def feedMiddleman(errorOrEvent: Either[Throwable, In]): Option[Out] = {
		for {
			handler <- middlemanHandler
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
					middlemanHandler = None
				}
				// Extract the wrapped result
				wrappedResult match {
					case Some(downstreamResult) => downstreamResult
					case None => None
				}
			}
		} yield {
			// If we're emitting a result, then the wrapper is definitely done, so clear it before returning
			middlemanHandler = None
			downstreamResult
		}
	}

	protected def feedMiddlemanEOF(): Option[Out] = {
		for {
			handler <- middlemanHandler
			if !handler.isFinished
			// triggering an end on the inner transformer may feed an event to the downstream and produce a result
			downstreamResult <- {
				middlemanHandler = None
				try handler.handleEnd() catch {
					case NonFatal(err) =>
						throw new Exception(s"Error sending [<Context End>] to the inner transformer [$handler] while running $debugName", err)
				}
			}
		} yield {
			middlemanHandler = None
			downstreamResult
		}
	}

}