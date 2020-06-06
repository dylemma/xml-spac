package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

/** Handler that will produce the `result` in response to any input or end event,
	* and will re-throw any error that gets passed.
	*
	* @param result The result value which will be produced
	*/
class ConstantHandler[T](result: T) extends Handler[Any, T] {
	override def toString = s"Constant($result)"

	private var didEmit = false
	def isFinished: Boolean = didEmit

	private def emitWith[T](result: => T) = try result finally didEmit = true
	def handleInput(input: Any): Option[T] = emitWith{ Some(result) }
	def handleError(err: Throwable): Option[T] = throw err
	def handleEnd(): T = emitWith{ result }
}
