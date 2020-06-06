package io.dylemma.spac.old.handlers

import io.dylemma.spac.old.Handler

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}

class FallbackSetHandler[In, Out](_handlers: Handler[In, Out]*) extends Handler[In, Out] with CacheFinished {
	override def toString = s"FallbackSetHandler(${_handlers mkString " OR "})"

	private val handlers = ArrayBuffer(_handlers: _*)
	private val isDead = handlers.map(_ => false)

	// Throwing errors may cause a handler to finish, but since the point of this combination
	// is to allow for one handler to act as the fallback for the other, we aren't actually
	// finished unless a *real* result was produced from one of the handlers, or if
	// both of them threw an error.
	protected def checkIsFinished = {
		val anyReallyFinished = handlers.iterator.zip(isDead.iterator).exists{ case (h, died) => h.isFinished && !died }
		anyReallyFinished || isDead.forall(identity)
	}

	def handleInput(input: In) = {
		val results = sendAll(_ handleInput input)
		findBestResult[Option[Out]](results, _.isDefined)
	}
	def handleError(error: Throwable) = {
		val results = sendAll(_ handleError error)
		findBestResult[Option[Out]](results, _.isDefined)
	}
	def handleEnd() = {
		val results = sendAll(_.handleEnd())
		findBestResult[Out](results, _ => true)
	}

	protected def sendAll[T](f: Handler[In, Out] => T): Iterator[Try[T]] = {
		handlers.iterator.zipWithIndex.map { case (h, index) =>
			if(isDead(index)) None
			else Some {
				val t = Try(f(h))
				if(t.isFailure) isDead(index) = true
				t
			}
		}.flatten
	}

	protected def findBestResult[T](results: Iterator[Try[T]], isBest: T => Boolean): T = {
		@tailrec def recurse(prevResult: Option[T], prevException: Option[Throwable]): T = {
			if (results.hasNext) {
				results.next match {
					case Success(r) if isBest(r) => r
					case Success(r) => recurse(Some(r), prevException)
					case Failure(err) => recurse(prevResult, Some(err))
				}
			} else {
				// if we checked all of the results, we should have either a prevResult or prevException
				prevResult getOrElse {
					prevException match {
						case Some(caughtException) => throw caughtException
						// only happens if we call asdf on an empty iterator
						case None => throw new IllegalStateException("Attempted to send input after all inner handlers died")
					}
				}
			}
		}
		recurse(None, None)
	}


}
