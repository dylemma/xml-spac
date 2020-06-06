package io.dylemma.spac.old

/** The base processing unit for pull-style streams processing.
  *
  * A handler accepts inputs of type `In`, eventually yielding
  * a value of type `Out`. If the `handleInput` or `handleError`
  * methods for a handler ever return a `Some`, it implies that
  * the handler has reached a result, and should no longer be called.
  * Similarly, if the `isFinished` method returns `true`, the
  * `handleX` methods should no longer be called.
  *
  * Handlers are mutable by nature. Handler instances should not
  * be shared across threads. Several traits in this library act
  * as "blueprints" which create handler instances. These blueprint
  * classes are immutable, and may be safely shared across threads.
  *
  * @tparam In  The input type
  * @tparam Out The output type
  */
trait Handler[-In, +Out] {

	/** Tells if this handler is finished.
	  *
	  * If a handler is finished, its `handleX` methods
	  * should no longer be called. Doing so after the
	  * handler finishes will cause undefined behavior.
	  * Typically a handler should be discarded once it
	  * is finished.
	  *
	  * @return whether this handler is finished
	  */
	def isFinished: Boolean

	/** Causes this handler to process an input element.
	  *
	  * This method should not be called if `isFinished == true`
	  *
	  * @param input an element to be processed
	  * @return An option containing the final output if
	  *         this handler became finished as a result
	  *         of processing the `input`, or `None` if
	  *         the handler is ready for more input.
	  */
	def handleInput(input: In): Option[Out]

	/** Causes this handler to process an error.
	  *
	  * This method should not be called if `isFinished == true`
	  *
	  * @param error an error to be processed
	  * @return An option containing the final output if
	  *         this handler became finished as a result
	  *         of processing the `error`, or `None` if
	  *         the handler is ready for more input.
	  */
	def handleError(error: Throwable): Option[Out]

	/** Causes this handler to finalize and produce a result.
	  *
	  * This method should not be called if `isFinished` is already true.
	  *
	  * @return The final output of this parser
	  */
	def handleEnd(): Out
}
