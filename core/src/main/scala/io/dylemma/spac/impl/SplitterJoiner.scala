package io.dylemma.spac
package impl

import scala.util.control.NonFatal

case class SplitterJoiner[In, C, Out](getTransformer: ContextPush[In, C] => Transformer[In, Out]) extends Transformer[Either[ContextChange[In, C], In], Out] {
	def newHandler = new SplitterJoiner.Handler(getTransformer, None)
}

object SplitterJoiner {

	case class MatchedState[In, Out](startTrace: ContextTrace[In], inner: Option[Transformer.Handler[In, Out]], extraDepth: Int) {
		def addDepth(delta: Int) = this.copy(extraDepth = extraDepth + delta)
	}

	class Handler[In, C, Out](
		getTransformer: ContextPush[In, C] => Transformer[In, Out],
		state: Option[SplitterJoiner.MatchedState[In, Out]]
	)
		extends Transformer.Handler[Either[ContextChange[In, C], In], Out]
	{
		private def continueUnmatched = Some(new Handler[In, C, Out](getTransformer, None))
		private def continueMatched(s: MatchedState[In, Out]) = Some(new Handler[In, C, Out](getTransformer, Some(s)))

		def step(in: Either[ContextChange[In, C], In]) = in match {
			case Right(in) =>
				state match {
					case Some(ms@MatchedState(startTrace, Some(inner), extraDepth)) =>
						// feed the input to the inner transformer
						val stepResult =
							try inner.step(in)
							catch { case NonFatal(e) => throw SpacException.ContextualizedException(startTrace, e) }
						stepResult match {
							case (emit, Some(`inner`)) => emit -> Some(this)
							case (emit, nextInner) => emit -> continueMatched(ms.copy(inner = nextInner))
						}

					case Some(matchedStateNoInner) =>
						// inner transformer must have ended, so ignore inputs until we exit the match
						Emit.nil -> Some(this)

					case None =>
						// ignore all inputs while not in a matched state
						Emit.nil -> Some(this)

				}

			case Left(push@ContextPush(trace, context)) =>
				state match {
					case Some(ms) =>
						// if we were already in a match, this push can be ignored,
						// but we need to update the `extraDepth` so we don't leave the matched state as soon as we see the next Pop
						Emit.nil -> continueMatched(ms.addDepth(1))

					case None =>
						// entering a new context, time to start a new transformer
						val nextTransformer =
							try getTransformer(push)
							catch { case NonFatal(e) => throw SpacException.ContextualizedException(trace, e) }

						Emit.nil -> continueMatched(MatchedState(trace, Some(nextTransformer.newHandler), 0))
				}

			case Left(ContextPop) =>
				state match {
					case Some(MatchedState(trace, innerOpt, 0)) =>
						// popping out of our matched state; inner transformer needs to be finished if it hasn't already
						val finalOuts = innerOpt match {
							case None => Emit.nil
							case Some(inner) =>
								try inner.finish()
								catch { case NonFatal(e) => throw SpacException.ContextualizedException(trace, e) }
						}
						finalOuts -> continueUnmatched

					case Some(ms) =>
						// a pop corresponding to an extra push. In practice this shouldn't happen
						Emit.nil -> continueMatched(ms.addDepth(-1))

					case None =>
						// a pop while we're not in any matched state can be ignored... in practice it won't happen
						Emit.nil -> Some(this)
				}
		}
		def finish() = state match {
			case None =>
				// clean exit, no extra actions taken
				Emit.nil

			case Some(MatchedState(_, None, _)) =>
				// in a match, but the inner transformer had finished, so no extra actions taken
				Emit.nil

			case Some(MatchedState(trace, Some(inner), _)) =>
				// in a matched context with an inner transformer left unfinished
				try inner.finish()
				catch { case NonFatal(e) => throw SpacException.ContextualizedException(trace, e) }
		}
	}
}