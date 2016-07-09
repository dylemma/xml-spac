package io.dylemma.xsp.handlers

import io.dylemma.xsp.Handler

import scala.util.{Failure, Success, Try}

/** Mapping handler which applies a transformation function to the results coming from the inner handler.
	*
	* @param f The transformation function
	* @param inner The inner handler
	*/
class MappedConsumerHandler[In, A, B](f: A => B, inner: Handler[In, A]) extends Handler[In, B] {
	override def toString = s"($inner).map($f)"

	def isFinished: Boolean = inner.isFinished
	def handleEnd(): B = f(inner.handleEnd())
	def handleInput(input: In): Option[B] = inner.handleInput(input).map(f)
	def handleError(error: Throwable): Option[B] = inner.handleError(error).map(f)
}

/** Mapping handler which applies a transformation function to inputs before passing them to the inner handler.
	*
	* @param f The transformation function
	* @param inner The inner handler
	*/
class MappedTransformerHandler[A, B, Out](f: A => B, inner: Handler[B, Out]) extends Handler[A, Out] {
	override def toString = s"Map($f) >> $inner"

	def isFinished: Boolean = inner.isFinished
	def handleEnd(): Out = inner.handleEnd()
	def handleInput(input: A): Option[Out] = {
		Try { f(input) } match {
			case Success(mappedInput) => inner.handleInput(mappedInput)
			case Failure(err) => inner.handleError(err)
		}
	}
	def handleError(error: Throwable): Option[Out] = inner.handleError(error)
}