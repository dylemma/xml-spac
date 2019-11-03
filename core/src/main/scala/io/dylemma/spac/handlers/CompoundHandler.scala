package io.dylemma.spac.handlers

import io.dylemma.spac.{Handler, debug}

import scala.reflect.ClassTag
import scala.collection.IndexedSeq

/** A composition of a number of "inner" handlers which delegates each
	* event to each (unfinished) inner handler, then reforms a final result
	* based on the final results of each of the inner handlers.
	*
	* Used internally by `ParserChain`
	*
	* @param innerHandlers An indexed collection if inner handlers. All events
	*                      passed through the CompoundHandler will be passed
	*                      through each (unfinished) innerHandler.
	* @param reform A function to convert the result values of the inner
	*               handlers to some concrete value. The input to this function
	*               will be an array containing results from the inner handlers.
	*               The result for `innerHandlers(n)` will be located at the n'th
	*               index in the array. Note that the array will have values of
	*               type `B`, but actual values are likely intended to have more
	*               specific types, so the `reform` function will likely need to
	*               perform class casting.
	* @tparam In The input type for each of the inner handlers
	* @tparam B A base type for the results of all of the innerHandlers.
	*           This will typically be `Any` or `Result[Any]`.
	* @tparam Out The type of the final combined result
	*/
class CompoundHandler[In, B: ClassTag, Out](
	innerHandlers: IndexedSeq[Handler[In, B]],
	reform: IndexedSeq[B] => Out
) extends Handler[In, Out] {

	private var pendingCount = innerHandlers.length
	private var didSendResult = false
	private val results = Array.tabulate[Option[B]](innerHandlers.length)(_ => None)

	override def toString = innerHandlers.mkString("Compound(", " ~ ", ")")

	def isFinished: Boolean = didSendResult && (pendingCount == 0)

	/** Creates the result by calling `reform` on the `results` array.
		* Assumes that all of the entries in the `results` array are defined.
		* Sets the `didSendResult` flag to `true`, so `isFinished` will return
		* true after calling this.
		*/
	private def sendResult() = {
		val resultView = results.map(_.get)
		val result = reform(resultView)
		didSendResult = true
		result
	}

	/** Feed an input or error to each of the handlers that is still unfinished
		* and has no result. This action may decrement the `pendingCount` and make
		* modifications to the `results` array, when a handler returns a `Some`.
		*
		* @param f A function to call on the handler, telling it to call
		*          `handleInput` or `handleError`
		*/
	@inline private def feedHandlers(f: Handler[In, B] => Option[B]) = {
		for {
			i <- innerHandlers.indices
			if results(i).isEmpty
			handler = innerHandlers(i) if !handler.isFinished
			hOutOpt = f(handler)
			hOut <- hOutOpt
		} {
			results(i) = hOutOpt
			pendingCount -= 1
			if (pendingCount < 0) {
				debug("[PROBLEM] pending count in compound handler reached below 0")
				pendingCount = 0
			}
		}
	}

	def handleInput(input: In): Option[Out] = {
		if (isFinished) None
		else {
			feedHandlers(_ handleInput input)
			if (pendingCount == 0) Some {sendResult()}
			else None
		}
	}

	def handleError(error: Throwable): Option[Out] = {
		if (isFinished) None
		else {
			feedHandlers(_ handleError error)
			if (pendingCount == 0) Some {sendResult()}
			else None
		}
	}

	def handleEnd(): Out = {
		if (isFinished) throw new IllegalStateException("handleEnd() called after finish")
		for {
			i <- innerHandlers.indices
			if results(i).isEmpty
			handler = innerHandlers(i) if !handler.isFinished
		} results(i) = Some {handler.handleEnd()}
		sendResult()
	}
}
