package io.dylemma.spac.handlers

import io.dylemma.spac.{Handler, Transformer}

import scala.annotation.tailrec

/** A handler for a collection of transformers that will be run in parallel.
  * Each event passed through this handler will be forwarded to each of the
  * respective handlers for the transformers in `toMerge`, referred to as "members".
  * If at any point a member finishes, it will no longer receive inputs.
  * If at any point *all* of the members finish, an early end will be passed to the downstream handler.
  * If at any point the downstream handler emits a result, that result will be returned and the members will be deactivated.
  *
  * @param downstream The receiving handler for the members
  * @param toMerge A list of transformers which will be run in parllel, passing outputs to the `downstream` handler
  * @tparam In The input type
  * @tparam A The common output type of the transformers, and the input type for the downstream handler.
  * @tparam Out The output type
  */
class ParallelTransformerHandler[In, A, Out](downstream: Handler[A, Out], toMerge: List[Transformer[In, A]]) extends Handler[In, Out] {
	protected object IgnoredEnd
	protected type WrappedResult = Either[Out, IgnoredEnd.type]
	protected type Member = Handler[In, WrappedResult]

	/** A guard around the downstream handler to prevent the `toMerge` transformers from directly calling
	  * `handleEnd` on the downstream handler. Transformers like `Take(N)` - that will feed an End event
	  * to the downstream when they've encountered an "early end" scenario - would potentially cause an
	  * early end even if other transformers in the merge were still ready to continue.
	  */
	protected val endGuardWrapper = new Handler[A, WrappedResult] {
		def isFinished = downstream.isFinished
		def handleInput(input: A) = downstream.handleInput(input) map { Left(_) }
		def handleError(error: Throwable) = downstream.handleError(error) map { Left(_) }
		def handleEnd() = Right(IgnoredEnd)
	}
	/** Every one of the transformers in `toMerge` needs to output to the same `downstream` handler.
	  */
	protected val members: List[Member] = toMerge map { _ makeHandler endGuardWrapper }

	// If the downstream handler doesn't want any more input, then we're done.
	// Otherwise, only finish if *every* transformer is finished.
	// We'll guard against the "only some are finished" scenario while handling inputs.
	def isFinished = downstream.isFinished || members.forall(_.isFinished)

	protected def runMembers(f: Member => Option[WrappedResult]): Option[Out] = {
		// Pass the input to all of the unfinished members.
		// If at any point one of the members returns a result, then we're done and can exit immediately.
		// If we find all members to be finished already, then we pass an "end" event to the downstream.
		@tailrec def recurse(allFinished: Boolean, remaining: List[Member]): (Boolean, Option[Out]) = remaining match {
			case Nil => (allFinished, None)
			case member :: tail if member.isFinished => recurse(allFinished, tail)
			case member :: tail =>
				f(member) match {
					// if an individual member calls `handleEnd`, we'll get this,
					// but the other members might not be done, so treat it as a noop
					case Some(Right(IgnoredEnd)) => recurse(allFinished, tail)
					// an actual result from the downstream
					case Some(Left(result)) => (allFinished, Some(result))
					case None => recurse(false, tail)
				}
		}
		val (allFinished, resultOpt) = recurse(true, members)
		resultOpt orElse {
			if(allFinished) Some(downstream.handleEnd())
			else None
		}
	}

	def handleInput(input: In): Option[Out] = runMembers(_ handleInput input)

	def handleError(error: Throwable) = runMembers(_ handleError error)

	def handleEnd() = {
		// Trigger an "end" on each of the handlers that aren't already finished,
		// in order to "flush" any leftover state they might have. This won't
		// send an end to the downstream handler, thanks to our guard, but the
		// `handleEnd` method *may* cause other input/errors to be sent to the
		// downstream. If any of these cause the downstream to emit a result,
		// we take that result instead of continuing.
		@tailrec def recurse(remaining: List[Member]): Option[Out] = remaining match {
			case Nil => None
			case member :: tail =>
				if(member.isFinished) recurse(tail)
				else {
					member.handleEnd() match {
						// the guard prevented `handleEnd` from reaching downstream; continue to the next member
						case Right(IgnoredEnd) => recurse(tail)
						// the member's end passed an input/error that caused the downstream to emit a result; return it
						case Left(result) => Some(result)
					}
				}
		}
		recurse(members) getOrElse {
			// if none of the members managed to trigger a result, finally pass the end to the downstream
			downstream.handleEnd()
		}
	}
}
