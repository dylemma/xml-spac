package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

import scala.util.{Failure, Success, Try}

/** Handler that delegates to an `inner` handler, converting result-typed inputs
	* to the corresponding `handleInput` or `handleError` call.
	*/
class UnwrapSafeTransformerHandler[A, B](downstream: Handler[A, B]) extends Handler[Try[A], B] {
	override def toString = s"UnwrapSafe >> $downstream"
	def isFinished: Boolean = downstream.isFinished
	def handleEnd(): B = downstream.handleEnd()
	def handleInput(input: Try[A]): Option[B] = input match {
		case Success(a) => downstream.handleInput(a)
		case Failure(e) => downstream.handleError(e)
	}
	def handleError(error: Throwable): Option[B] = downstream.handleError(error)
}

/** Handler that upgrades events from the upstream to `Results` before passing them
	* to the inner handler. This way, the inner handler will never have its `handleError`
	* method called.
	*/
class WrapSafeTransformerHandler[A, B](downstream: Handler[Try[A], B]) extends Handler[A, B] {
	override def toString = s"WrapSafe >> $downstream"
	def isFinished = downstream.isFinished
	def handleEnd = downstream.handleEnd()
	def handleInput(input: A) = downstream.handleInput(Success(input))
	def handleError(err: Throwable) = downstream.handleInput(Failure(err))
}