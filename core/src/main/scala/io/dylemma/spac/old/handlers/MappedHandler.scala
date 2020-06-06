package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

/** Mapping handler which applies a transformation function to the results coming from the wrapped handler.
	*
	* @param f The transformation function
	* @param inner The wrapped handler
	*/
class MappedConsumerHandler[In, A, B](f: A => B, inner: Handler[In, A]) extends Handler[In, B] {
	override def toString = s"($inner).map($f)"

	def isFinished: Boolean = inner.isFinished
	def handleEnd(): B = f(inner.handleEnd())
	def handleInput(input: In): Option[B] = inner.handleInput(input).map(f)
	def handleError(error: Throwable): Option[B] = inner.handleError(error).map(f)
}

/** Mapping handler which applies a transformation function to inputs before passing them to the downstream handler.
	*
	* @param f The transformation function
	* @param downstream The downstream handler
	*/
class MappedTransformerHandler[A, B, Out](f: A => B, val downstream: Handler[B, Out]) extends TransformerHandler[A, B, Out] {
	override def toString = s"Map($f) >> $downstream"
	protected def transformInput(input: A): Option[B] = Some(f(input))
}

