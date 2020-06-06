package io.dylemma.spac.impl

import cats.data.Chain
import cats.{Applicative, ApplicativeError}
import io.dylemma.spac._

object StackMatchSplitter {
	def apply[F[+_], In, Elem, C](matcher: ContextMatcher[Elem, C])(implicit F: Applicative[F]) = {
		new StackMatchSplitter[F, In, Elem, C](matcher, Unmatched[In, Elem](Vector.empty, Vector.empty))
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
class StackMatchSplitter[F[+_], In, Elem, C]
	(matcher: ContextMatcher[Elem, C], state: StackMatchSplitter.State[In, Elem])
	(implicit F: Applicative[F])
	extends Transformer[F, Either[ContextChange[In, Elem], In], Either[ContextChange[In, C], In]]
{
	import StackMatchSplitter._
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
					F.pure(Emit.nil -> Some(new StackMatchSplitter(matcher, Matched(extraDepth + 1, prev))))
				case current @ Unmatched(prevTraces, prevStack) =>
					// the push may be enough to put us into a matching state; we need to check the `matcher`
					val nextTraces = prevTraces :+ incomingTrace
					val nextStack = prevStack :+ elem
					matcher(nextStack, 0, nextStack.length) match {
						case None =>
							// no new context match; update the unmatched state
							F.pure(Emit.nil -> Some(new StackMatchSplitter(matcher, Unmatched(nextTraces, nextStack))))

						case Some(c) =>
							// new matched context!
							// rem is the piece of the stack that wasn't consumed by the match.
							val combinedTrace = nextTraces.foldLeft(ContextTrace[In](Chain.empty))(_ / _)
							val change = ContextPush(combinedTrace, c)
							F.pure(Emit.one(Left(change)) -> Some(new StackMatchSplitter(matcher, Matched(0, current))))
					}
			}

		case Left(ContextPop) =>
			state match {
				case Matched(0, prev) =>
					// popping out of the matched context
					F.pure(Emit.one(Left(ContextPop)) -> Some(new StackMatchSplitter(matcher, prev)))

				case Matched(extraDepth, prev) =>
					// decrement the `extraDepth` state but otherwise no change
					F.pure(Emit.nil -> Some(new StackMatchSplitter(matcher, Matched(extraDepth - 1, prev))))

				case Unmatched(traces, stack) =>
					// not in a matched context; just pop the stack tracking states
					val poppedTraces = traces.dropRight(1)
					val poppedStack = stack.dropRight(1)
					F.pure(Emit.nil -> Some(new StackMatchSplitter(matcher, Unmatched(poppedTraces, poppedStack))))
			}
	}

	def finish: F[Emit[Either[ContextChange[In, C], In]]] = {
		// if we're in a matched context, it needs to be closed (via ContextPop) before we finish
		val toEmit = if(state.isMatched) Emit.one(Left(ContextPop)) else Emit.nil
		F.pure(toEmit)
	}
}

// --------------------------------------------

object SplitterJoiner {
	def apply[F[+_], In, C, Out](getTransformer: ContextPush[In, C] => Transformer[F, In, Out])(implicit F: ApplicativeError[F, Throwable]) = {
		new SplitterJoiner[F, In, C, Out](getTransformer, None)
	}

	case class MatchedState[F[+_], In, Out](startTrace: ContextTrace[In], inner: Option[Transformer[F, In, Out]], extraDepth: Int) {
		def addDepth(delta: Int) = this.copy(extraDepth = extraDepth + delta)
	}
}

class SplitterJoiner[F[+_], In, C, Out](
	getTransformer: ContextPush[In, C] => Transformer[F, In, Out],
	state: Option[SplitterJoiner.MatchedState[F, In, Out]]
)(implicit F: ApplicativeError[F, Throwable])
	extends Transformer[F, Either[ContextChange[In, C], In], Out]
{
	import SplitterJoiner._
	import cats.implicits._

	private def continueUnmatched = Some(new SplitterJoiner[F, In, C, Out](getTransformer, None))
	private def continueMatched(s: MatchedState[F, In, Out]) = Some(new SplitterJoiner[F, In, C, Out](getTransformer, Some(s)))

	def step(in: Either[ContextChange[In, C], In]): F[(Emit[Out], Option[Transformer[F, Either[ContextChange[In, C], In], Out]])] = in match {
		case Right(in) =>
			state match {
				case Some(ms @ MatchedState(startTrace, Some(inner), extraDepth)) =>
					// feed the input to the inner transformer
					inner.step(in)
						.adaptErr { case e: Exception => ContextualizedException(startTrace, e) }
						.map {
							case (emit, Some(`inner`)) => emit -> Some(this)
							case (emit, nextInner) => emit -> continueMatched(ms.copy(inner = nextInner))
						}

				case Some(matchedStateNoInner) =>
					// inner transformer must have ended, so ignore inputs until we exit the match
					F.pure(Emit.nil -> Some(this))

				case None =>
					// ignore all inputs while not in a matched state
					F.pure(Emit.nil -> Some(this))

			}

		case Left(push @ ContextPush(trace, context)) =>
			state match {
				case Some(ms) =>
					// if we were already in a match, this push can be ignored,
					// but we need to update the `extraDepth` so we don't leave the matched state as soon as we see the next Pop
					F.pure(Emit.nil -> continueMatched(ms.addDepth(1)))

				case None =>
					// entering a new context, time to start a new transformer
					F.catchNonFatal(getTransformer(push))
						.adaptErr { case e: Exception => ContextualizedException(trace, e) }
						.map { inner => Emit.nil -> continueMatched(MatchedState(trace, Some(inner), 0)) }
			}

		case Left(ContextPop) =>
			state match {
				case Some(MatchedState(trace, innerOpt, 0)) =>
					// popping out of our matched state; inner transformer needs to be finished if it hasn't already
					val finalOuts = innerOpt match {
						case None => F.pure(Emit.nil)
						case Some(inner) => inner.finish.adaptErr { case e: Exception => ContextualizedException(trace, e) }
					}
					finalOuts.map(_ -> continueUnmatched)

				case Some(ms) =>
					// a pop corresponding to an extra push. In practice this shouldn't happen
					F.pure(Emit.nil -> continueMatched(ms.addDepth(-1)))

				case None =>
					// a pop while we're not in any matched state can be ignored... in practice it won't happen
					F.pure(Emit.nil -> Some(this))
			}
	}
	def finish: F[Emit[Out]] = state match {
		case None =>
			// clean exit, no extra actions taken
			F.pure(Emit.nil)

		case Some(MatchedState(_, None, _)) =>
			// in a match, but the inner transformer had finished, so no extra actions taken
			F.pure(Emit.nil)

		case Some(MatchedState(trace, Some(inner), _)) =>
			// in a matched context with an inner transformer left unfinished
			inner.finish.adaptErr { case e: Exception => ContextualizedException(trace, e) }
	}
}