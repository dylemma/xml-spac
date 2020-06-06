package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.{Handler, Parser, Transformer}

import scala.util.{Failure, Success, Try}

/** Part of the `Splitter` implementation responsible for creating an `In => P => Out` chain for a downstream handler.
  * Implementations will create handlers that consume an stream of events by forwarding some transformation of those
  * events to the downstream handler. E.g. by consuming the stream to a P and sending the `P` values downstream, or
  * by transforming the stream to a series of `P` values and sending those `P` values downstream.
  *
  * @tparam Context
  * @tparam In
  * @tparam P
  */
trait ContextMiddleman[Context, In, P] {
	/** Create a wrapper for the downstream handler based on a context.
	  * The wrapper's job is to intercept events (even EOF), interpreting them in some way, and forwarding
	  * results to the downstream handler.
	  *
	  * The wrapper handler should only return `Some` from *any* of its `handleX` methods if the *downstream*
	  * handler returned a `Some`. It is allowed to become finished without returning `Some`, e.g. if the
	  * wrapper serves as a Take(n) transformer, or some other transformer that ends early.
	  *
	  * But the idea is that the middleman wrapper is disposable, with a new one created for each new context.
	  * Whatever handler is managing creation of contexts (and therefore calling this method) is responsible
	  * for sending a final EOF signal to the downstream handler.
	  *
	  * @param context
	  * @param downstream
	  * @tparam Out
	  * @return
	  */
	def createWrapper[Out](context: Context, downstream: Handler[P, Out]): Handler[In, Option[Out]]
}

/** HandlerMiddleman that runs a consumer on all inputs until that consumer returns a result, which will be sent downstream.
  *
  * @param consumerForContext
  * @tparam Context
  * @tparam In
  * @tparam P
  */
class ContextConsumerMiddleman[Context, In, P](consumerForContext: Context => Parser[In, P]) extends ContextMiddleman[Context, In, P] {
	override def toString = consumerForContext.toString

	def createWrapper[Out](context: Context, downstream: Handler[P, Out]) = new Handler[In, Option[Out]] {
		val innerHandler = consumerForContext(context).makeHandler()
		def isFinished = innerHandler.isFinished || downstream.isFinished

		protected def feedDownstream(innerResult: Try[Option[P]]) = innerResult match {
			case Success(resultOpt) => resultOpt.flatMap(downstream.handleInput).map(Some(_))
			case Failure(err) => downstream.handleError(err).map(Some(_))
		}

		def handleInput(input: In): Option[Option[Out]] = {
			feedDownstream(Try { innerHandler handleInput input })
		}
		def handleError(error: Throwable): Option[Option[Out]] = {
			feedDownstream(Try { innerHandler.handleError(error) })
		}
		def handleEnd(): Option[Out] = {
			val innerResult = innerHandler.handleEnd()
			downstream.handleInput(innerResult)
		}
	}
}

/** HandlerMiddleman that acts as a transformer, but guards against the EOF signal being sent via the transformer.
  *
  * @param transformerForContext
  * @tparam Context
  * @tparam In
  * @tparam P
  */
class ContextTransformerMiddleman[Context, In, P](transformerForContext: Context => Transformer[In, P]) extends ContextMiddleman[Context, In, P] {
	override def toString = transformerForContext.toString()

	def createWrapper[Out](context: Context, downstream: Handler[P, Out]) = {
		transformerForContext(context).makeHandler(new Handler[P, Option[Out]] {
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
	}
}