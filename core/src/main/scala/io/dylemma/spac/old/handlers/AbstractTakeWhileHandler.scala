package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

abstract class AbstractTakeWhileHandler[In, Out](protected val inner: Handler[In, Out]) extends Handler[In, Out] {
	protected def shouldBeFinished: Boolean
	protected def feedInput(input: In): Option[Out]

	private var didFeedEnd = false
	private def feedEnd(): Out = {
		didFeedEnd = true
		inner.handleEnd()
	}
	def isFinished = inner.isFinished || (shouldBeFinished && didFeedEnd)
	def handleInput(input: In): Option[Out] = {
		if(isFinished) None
		else if(shouldBeFinished && !didFeedEnd) Some(feedEnd())
		else {
			val feedResult = if(!shouldBeFinished) feedInput(input) else None
			if(inner.isFinished) feedResult
			else if(shouldBeFinished) Some(feedEnd())
			else feedResult
		}
	}
	def handleEnd(): Out = {
		if(isFinished) throw new IllegalStateException("handleEnd() called after finish")
		else feedEnd()
	}
	def handleError(err: Throwable): Option[Out] = {
		if(isFinished) None
		else inner.handleError(err)
	}
}
