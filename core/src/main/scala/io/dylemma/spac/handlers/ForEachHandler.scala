package io.dylemma.spac.handlers

import io.dylemma.spac.Handler

class ForEachHandler[A](f: A => Any) extends Handler[A, Unit] with ManualFinish {
	override def toString = s"ForEach($f)"
	def handleInput(input: A) = {
		if(!isFinished) f(input)
		None
	}
	def handleError(error: Throwable) = finishWith { throw error }
	def handleEnd() = finishWith{ () }
}


class SideEffectHandler[A, Out](f: A => Any, val downstream: Handler[A, Out]) extends TransformerHandler[A, A, Out]
{
	override def toString = s"SideEffect($f) >> $downstream"
	protected def transformInput(input: A): Option[A] = {
		f(input)
		Some(input)
	}
}