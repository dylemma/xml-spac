package io.dylemma.spac

import scala.util.Try

/** Extension of the `Handler` trait that additionally handles entering and exiting a context.
  * A separate component should be responsible for deciding when a context begins or ends.
  * The combination of that decider and this handler forms the basis of a `Splitter`'s handler.
  *
  * @tparam In  The input type
  * @tparam Context The context type
  * @tparam Out The output type
  */
trait ContextSensitiveHandler[-In, -Context, +Out] extends Handler[In, Out] {

	/** Causes this handler to process a context start event.
	  *
	  * This method should not be called if `isFinished == true`
	  *
	  * @param context A `Try` containing the new context, or an error originating
	  *                from the "context decider"
	  * @return An option containing the final output if this handler became
	  *         finished as a result of entering the new context, or `None`
	  *         if the handler is ready for more input.
	  */
	def handleContextStart(context: Try[Context]): Option[Out]

	/** Causes this handler to process a context end.
	  *
	  * This method should not be called if `isFinished == true`, or
	  * if no corresponding `handleContextStart` was called.
	  *
	  * @return An option containing the final output if this handler
	  *         became finished as a result of leaving the context, or
	  *         `None` if the handler is ready for more input.
	  */
	def handleContextEnd(): Option[Out]
}
