package io.dylemma.spac
package impl

import cats.data.Chain

case class SplitterByContextMatch[In, Elem, C](matcher: ContextMatcher[Elem, C])(implicit S: StackLike[In, Elem]) extends Splitter[In, C] {

	def addBoundaries = S.interpret :>> new SplitterByContextMatch.Boundaries(matcher)
	def newHandler = new SplitterByContextMatch.Handler(matcher, SplitterByContextMatch.Unmatched[In, Elem](Vector.empty, Vector.empty))
}

object SplitterByContextMatch {

	class Boundaries[In, Elem, C](matcher: ContextMatcher[Elem, C])
		extends Transformer[Either[ContextChange[In, Elem], In], Either[ContextChange[In, C], In]]
	{
		def newHandler = new SplitterByContextMatch.Handler(matcher, SplitterByContextMatch.Unmatched[In, Elem](Vector.empty, Vector.empty))
	}

	sealed abstract class State[In, Elem](val isMatched: Boolean)
	case class Unmatched[In, Elem](traces: Vector[ContextTrace[In]], stack: Vector[Elem]) extends State[In, Elem](false)
	case class Matched[In, Elem](extraDepth: Int, prev: Unmatched[In, Elem]) extends State[In, Elem](true)

	/** Splitter implementation detail that re-interprets stack states into contexts.
	  * As stack elements are pushed and popped via the incoming stream,
	  * it plugs the accumulated stack into the given `matcher` to determine if a
	  * matched context has been entered (or exited). The matched context is
	  * communicated as `ContextChange` events of the `C` type via the outgoing stream.
	  */
	class Handler[In, Elem, C](matcher: ContextMatcher[Elem, C], state: SplitterByContextMatch.State[In, Elem])
		extends Transformer.Handler[Either[ContextChange[In, Elem], In], Either[ContextChange[In, C], In]]
	{
		def step(in: Either[ContextChange[In, Elem], In]) = in match {
			case Right(in) =>
				/*
				// if we're inside the matched context, emit the value, otherwise ignore it
				val emit = if(state.isMatched) Emit.one(Right(in)) else Emit.empty
				// no state change, so we return this transformer
				F.pure(emit -> Some(this))
				*/
				// The above commented-out code could be used to filter out inputs while we're not in a matched context,
				// but I think that functionality shouldn't be the responsibility of this transformer.
				// It should be focused on transforming the ContextChange events and nothing else.
				Emit.one(Right(in)) -> Some(this)

			case Left(ContextPush(incomingTrace, elem)) =>
				state match {
					case Matched(extraDepth, prev) =>
						// we're already in a matched context, so the push simply increments our extraDepth
						Emit.nil -> Some(new Handler(matcher, Matched(extraDepth + 1, prev)))
					case current@Unmatched(prevTraces, prevStack) =>
						// the push may be enough to put us into a matching state; we need to check the `matcher`
						val nextTraces = prevTraces :+ incomingTrace
						val nextStack = prevStack :+ elem
						matcher(nextStack, 0, nextStack.length) match {
							case None =>
								// no new context match; update the unmatched state
								Emit.nil -> Some(new Handler(matcher, Unmatched(nextTraces, nextStack)))

							case Some(c) =>
								// new matched context!
								// rem is the piece of the stack that wasn't consumed by the match.
								val combinedTrace = nextTraces.foldLeft(ContextTrace[In](Chain.empty))(_ / _)
								val change = ContextPush(combinedTrace, c)
								Emit.one(Left(change)) -> Some(new Handler(matcher, Matched(0, current)))
						}
				}

			case Left(ContextPop) =>
				state match {
					case Matched(0, prev) =>
						// popping out of the matched context
						Emit.one(Left(ContextPop)) -> Some(new Handler(matcher, prev))

					case Matched(extraDepth, prev) =>
						// decrement the `extraDepth` state but otherwise no change
						Emit.empty -> Some(new Handler(matcher, Matched(extraDepth - 1, prev)))

					case Unmatched(traces, stack) =>
						// not in a matched context; just pop the stack tracking states
						val poppedTraces = traces.dropRight(1)
						val poppedStack = stack.dropRight(1)
						Emit.empty -> Some(new Handler(matcher, Unmatched(poppedTraces, poppedStack)))
				}
		}

		def finish() = {
			// if we're in a matched context, it needs to be closed (via ContextPop) before we finish
			if (state.isMatched) Emit.one(Left(ContextPop)) else Emit.nil
		}
	}
}

