package io.dylemma.spac
package impl

import cats.Applicative
import cats.data.Chain

object SplitterByContextMatch {
	def apply[F[+_], In, Elem, C](matcher: ContextMatcher[Elem, C])(implicit F: Applicative[F]) = {
		new SplitterByContextMatch[F, In, Elem, C](matcher, Unmatched[In, Elem](Vector.empty, Vector.empty))
	}

	sealed abstract class State[In, Elem](val isMatched: Boolean)
	case class Unmatched[In, Elem](traces: Vector[ContextTrace[In]], stack: Vector[Elem]) extends State[In, Elem](false)
	case class Matched[In, Elem](extraDepth: Int, prev: Unmatched[In, Elem]) extends State[In, Elem](true)
}

/** Splitter implementation detail that re-interprets stack states into contexts.
  * As stack elements are pushed and popped via the incoming stream,
  * it plugs the accumulated stack into the given `matcher` to determine if a
  * matched context has been entered (or exited). The matched context is
  * communicated as `ContextChange` events of the `C` type via the outgoing stream.
  */
class SplitterByContextMatch[F[+_], In, Elem, C](matcher: ContextMatcher[Elem, C], state: SplitterByContextMatch.State[In, Elem])(implicit F: Applicative[F])
	extends Transformer[F, Either[ContextChange[In, Elem], In], Either[ContextChange[In, C], In]]
{
	import SplitterByContextMatch._
	def step(in: Either[ContextChange[In, Elem], In]): F[(Emit[Either[ContextChange[In, C], In]], Option[Transformer[F, Either[ContextChange[In, Elem], In], Either[ContextChange[In, C], In]]])] = in match {
		case Right(in) =>
			/*
			// if we're inside the matched context, emit the value, otherwise ignore it
			val emit = if(state.isMatched) Emit.one(Right(in)) else Emit.nil
			// no state change, so we return this transformer
			F.pure(emit -> Some(this))
			*/
			// The above commented-out code could be used to filter out inputs while we're not in a matched context,
			// but I think that functionality shouldn't be the responsibility of this transformer.
			// It should be focused on transforming the ContextChange events and nothing else.
			F.pure(Emit.one(Right(in)) -> Some(this))

		case Left(ContextPush(incomingTrace, elem)) =>
			state match {
				case Matched(extraDepth, prev) =>
					// we're already in a matched context, so the push simply increments our extraDepth
					F.pure(Emit.nil -> Some(new SplitterByContextMatch(matcher, Matched(extraDepth + 1, prev))))
				case current@Unmatched(prevTraces, prevStack) =>
					// the push may be enough to put us into a matching state; we need to check the `matcher`
					val nextTraces = prevTraces :+ incomingTrace
					val nextStack = prevStack :+ elem
					matcher(nextStack, 0, nextStack.length) match {
						case None =>
							// no new context match; update the unmatched state
							F.pure(Emit.nil -> Some(new SplitterByContextMatch(matcher, Unmatched(nextTraces, nextStack))))

						case Some(c) =>
							// new matched context!
							// rem is the piece of the stack that wasn't consumed by the match.
							val combinedTrace = nextTraces.foldLeft(ContextTrace[In](Chain.empty))(_ / _)
							val change = ContextPush(combinedTrace, c)
							F.pure(Emit.one(Left(change)) -> Some(new SplitterByContextMatch(matcher, Matched(0, current))))
					}
			}

		case Left(ContextPop) =>
			state match {
				case Matched(0, prev) =>
					// popping out of the matched context
					F.pure(Emit.one(Left(ContextPop)) -> Some(new SplitterByContextMatch(matcher, prev)))

				case Matched(extraDepth, prev) =>
					// decrement the `extraDepth` state but otherwise no change
					F.pure(Emit.nil -> Some(new SplitterByContextMatch(matcher, Matched(extraDepth - 1, prev))))

				case Unmatched(traces, stack) =>
					// not in a matched context; just pop the stack tracking states
					val poppedTraces = traces.dropRight(1)
					val poppedStack = stack.dropRight(1)
					F.pure(Emit.nil -> Some(new SplitterByContextMatch(matcher, Unmatched(poppedTraces, poppedStack))))
			}
	}

	def finish: F[Emit[Either[ContextChange[In, C], In]]] = {
		// if we're in a matched context, it needs to be closed (via ContextPop) before we finish
		val toEmit = if (state.isMatched) Emit.one(Left(ContextPop)) else Emit.nil
		F.pure(toEmit)
	}
}