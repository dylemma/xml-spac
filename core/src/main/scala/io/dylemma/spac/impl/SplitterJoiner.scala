package io.dylemma.spac
package impl

import cats.ApplicativeError

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
						.adaptErr { case e: Exception => SpacException.ContextualizedException(startTrace, e) }
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
						.adaptErr { case e: Exception => SpacException.ContextualizedException(trace, e) }
						.map { inner => Emit.nil -> continueMatched(MatchedState(trace, Some(inner), 0)) }
			}

		case Left(ContextPop) =>
			state match {
				case Some(MatchedState(trace, innerOpt, 0)) =>
					// popping out of our matched state; inner transformer needs to be finished if it hasn't already
					val finalOuts = innerOpt match {
						case None => F.pure(Emit.nil)
						case Some(inner) => inner.finish.adaptErr { case e: Exception => SpacException.ContextualizedException(trace, e) }
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
			inner.finish.adaptErr { case e: Exception => SpacException.ContextualizedException(trace, e) }
	}
}