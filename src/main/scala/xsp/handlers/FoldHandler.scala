package xsp.handlers

import xsp.{Handler, Result}

import scala.util.control.NonFatal

class FoldHandler[A, R](init: R, f: (R, A) => R)
	extends Handler[A, Result[R]]
	with ManualFinish
	with FinishOnError
{
	private var state = init

	def handleInput(input: A): Option[Result[R]] = {
		try {
			state = f(state, input)
			None
		} catch {
			case NonFatal(err) => finishWith {
				Some(Result.Error(err))
			}
		}
	}
	def handleEnd(): Result[R] = finishWith(Result.Success(state))
}

class FoldResultsHandler[A, R](init: Result[R], f: (Result[R], Result[A]) => Result[R])
	extends Handler[A, Result[R]]
	with ManualFinish
{
	private var state = init

	private def advance(input: Result[A]) = {
		try {
			state = f(state, input)
			None
		} catch {
			// f is meant to properly handle error cases, so if it
			// throws, we'll just end the fold immediately
			case NonFatal(err) => finishWith {
				Some(Result.Error(err))
			}
		}
	}

	def handleInput(input: A): Option[Result[R]] = {
		advance(Result.Success(input))
	}
	def handleError(error: Throwable): Option[Result[R]] = {
		advance(Result.Error(error))
	}
	def handleEnd(): Result[R] = finishWith(state)
}
