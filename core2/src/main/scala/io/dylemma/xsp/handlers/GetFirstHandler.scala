package io.dylemma.xsp.handlers

import io.dylemma.xsp.{Handler, Result}

class GetFirstHandler[A]
	extends Handler[A, Result[A]]
	with ManualFinish
	with FinishOnError
{
	override def toString = "GetFirst"

	def handleInput(input: A): Option[Result[A]] = finishWith {
		Some(Result.Success(input))
	}
	def handleEnd(): Result[A] = finishWith {
		throw new NoSuchElementException(
			"encountered end of stream before the first element"
		)
	}
}

class GetFirstOptionHandler[A]
	extends Handler[A, Result[Option[A]]]
	with ManualFinish
	with FinishOnError
{
	override def toString = "GetFirstOption"

	def handleInput(input: A): Option[Result[Option[A]]] = finishWith {
		Some(Result.Success(Some(input)))
	}
	def handleEnd(): Result[Option[A]] = finishWith {
		Result.Success.none
	}
}