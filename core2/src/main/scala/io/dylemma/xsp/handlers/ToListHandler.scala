package io.dylemma.xsp.handlers

import io.dylemma.xsp.{Handler, Result}

class ToListHandler[A] extends Handler[A, List[A]] with ManualFinish {
	override def toString = "ToList"

	private val lb = List.newBuilder[A]
	def handleInput(input: A): Option[List[A]] = {
		lb += input
		None
	}
	def handleError(err: Throwable): Option[List[A]] = finishWith { throw err }
	def handleEnd(): List[A] = finishWith { lb.result() }
}
