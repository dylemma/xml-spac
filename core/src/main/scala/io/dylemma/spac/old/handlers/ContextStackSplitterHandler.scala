package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.{ContextSensitiveHandler, Handler, debug}
import io.dylemma.spac.types.Stackable
import io.dylemma.spac.ContextMatcher

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

/** Handler that tracks entry and exit from a stack-based context, notifying a downstream handler.
  *
  * @param matcher
  * @param downstream
  * @param stackable
  * @tparam In  The input type
  * @tparam StackElem
  * @tparam Context
  * @tparam Out The output type
  */
class ContextStackSplitterHandler[In, StackElem, Context, Out](
	matcher: ContextMatcher[StackElem, Context],
	downstream: ContextSensitiveHandler[In, Context, Out]
)(
	implicit stackable: Stackable.Aux[In, StackElem]
) extends Handler[In, Out] {

	protected def debugName = s"Splitter($matcher) >> $downstream"
	override def toString = debugName

	// EOF and Errors are proxied directly to the downstream
	def isFinished = downstream.isFinished
	def handleError(err: Throwable) = downstream.handleError(err)
	def handleEnd() = downstream.handleEnd()

	// context tracking state
	private val stack = new ArrayBuffer[StackElem]
	private var currentContext: Option[Try[Context]] = None
	private var matchStartDepth = 0

	// Inputs may cause a stack push/pop before/after forwarding the actual input
	def handleInput(input: In) = {
		stackable.asPush(input) match {
			// PUSH
			case Some(elem) =>
				// Push and Send, but the order depends on the Stackable
				if (stackable.order(input) >= 0) {
					performStackPush(elem) orElse sendInput(input)
				} else {
					sendInput(input) orElse performStackPush(elem)
				}

			// POP
			case None if stackable.isPop(input) =>
				// Pop and Send, but the order depends on the Stackable
				if (stackable.order(input) >= 0) {
					performStackPop() orElse sendInput(input)
				} else {
					sendInput(input) orElse performStackPop()
				}

			// NO CHANGE
			case _ =>
				sendInput(input)
		}
	}

	// Send an input downstream
	protected def sendInput(input: In): Option[Out] = downstream.handleInput(input)

	// Push to the stack.
	// Doing so may cause a new context to start.
	// If context matching fails, the error will be forwarded downstream,
	// possibly producing a result that would end this handler.
	protected def performStackPush(newElem: StackElem): Option[Out] = {
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
				result = downstream.handleContextStart(tryCtx)
			}
		}

		result
	}

	// Pop from the stack.
	// Doing so may end the current context, so we have to send EOF signals
	// and handle any results generated from doing so.
	protected def performStackPop(): Option[Out] = {
		stack.remove(stack.size - 1)

		if (stack.size < matchStartDepth) {
			matchStartDepth = 0
			debug(s"Left context: $currentContext")
			currentContext = None
			downstream.handleContextEnd()
		} else {
			None
		}
	}

}