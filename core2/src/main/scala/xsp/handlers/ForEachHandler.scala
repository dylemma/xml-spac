package xsp.handlers

import xsp.{Handler, Result}

import scala.util.Try

class ForEachHandler[A](f: A => Any)
	extends Handler[A, Unit]
	with ManualFinish
{
	override def toString = s"ForEach($f)"
	def handleEnd() = ()
	def handleError(error: Throwable) = throw error
	def handleInput(input: A) = {
		if(!isFinished) f(input)
		None
	}
}


class SideEffectHandler[A, Out](f: A => Any, next: Handler[A, Out])
	extends AbstractHandler(next)
{
	override def toString = s"SideEffect($f) >> $next"
	override def handleInput(input: A): Option[Out] = {
		Try(f(input)) match {
			case scala.util.Success(_) => next.handleInput(input)
			case scala.util.Failure(err) => next.handleError(err)
		}
	}
}