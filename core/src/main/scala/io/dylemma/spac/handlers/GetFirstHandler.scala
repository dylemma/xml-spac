package io.dylemma.spac.handlers

import io.dylemma.spac.{Handler, Result}

class GetFirstHandler[A] extends Handler[A, A] with ManualFinish {
	override def toString = "GetFirst"

	def handleInput(input: A): Option[A] = finishWith { Some(input) }
	def handleError(error: Throwable): Option[A] = finishWith { throw error }
	def handleEnd(): A = finishWith {
		throw new NoSuchElementException("encountered end of stream before the first element")
	}
}

class GetFirstOptionHandler[A] extends Handler[A, Option[A]] with ManualFinish {
	override def toString = "GetFirstOption"

	def handleInput(input: A): Option[Option[A]] = finishWith { Some(Some(input)) }
	def handleError(error: Throwable): Option[Option[A]] = finishWith { throw error }
	def handleEnd(): Option[A] = finishWith { None }
}