package xsp.handlers

import xsp.{Handler, Parser, Result}

abstract class AbstractParser[Context, Out] extends Parser[Context, Out] {
	def makeHandler(contextError: Throwable) = new OneShotHandler(Result.Error(contextError))
}

class OneShotHandler[Out](value: =>Out) extends Handler[Any, Out] {
	private var didEmit = false
	def isFinished: Boolean = didEmit

	private def emitWith[T](result: => T) = try result finally didEmit = true
	def handleInput(input: Any): Option[Out] = emitWith{ Some(value) }
	def handleError(err: Throwable): Option[Out] = emitWith{ Some(value) }
	def handleEnd(): Out = emitWith{ value }
}
