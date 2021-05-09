package io.dylemma.spac
package impl

import scala.collection.immutable.VectorBuilder
import scala.util.control.NonFatal

case class SplitterJoiner[In, C, Out](getTransformer: ContextPush[In, C] => Transformer[In, Out]) extends Transformer[Either[ContextChange[In, C], In], Out] {
	def newHandler = new SplitterJoiner.Handler(getTransformer)
}

object SplitterJoiner {

	case class MatchedState[In, Out](
		startTrace: ContextTrace[In],
		var inner: Option[Transformer.BoundHandler[In]],
		var extraDepth: Int
	)

	class Handler[In, C, Out](getTransformer: ContextPush[In, C] => Transformer[In, Out]) extends Transformer.Handler[Either[ContextChange[In, C], In], Out] {

		private var state: Option[MatchedState[In, Out]] = None
		private val innerTransformerOut = new Transformer.BoundHandler.ToBuilder(new VectorBuilder[Out])

		override def toString = state match {
			case Some(MatchedState(_, Some(inner), _)) => s"Joiner($inner)"
			case _ => "Joiner(<pending>)"
		}

		def push(in: Either[ContextChange[In, C], In], out: Transformer.HandlerWrite[Out]) = in match {
			case Right(in) =>
				state match {
					case Some(ms @ MatchedState(startTrace, Some(inner), extraDepth)) =>
						// feed the input to the inner transformer
						val innerSignal =
							try inner.push(in)
							catch { case NonFatal(e) => throw SpacException.addTrace(e, startTrace.asSpacTraceElems) }
						// if the inner transformer stopped, clear the reference to it in our state
						if (innerSignal.isStop) {
							ms.inner = None
						}
						out.pushMany(innerTransformerOut.take().iterator)

					case Some(matchedStateNoInner) =>
						// inner transformer must have ended, so ignore inputs until we exit the match
						Signal.Continue

					case None =>
						// ignore all inputs while not in a matched state
						Signal.Continue

				}

			case Left(push @ ContextPush(trace, context)) =>
				state match {
					case Some(ms) =>
						// if we were already in a match, this push can be ignored,
						// but we need to update the `extraDepth` so we don't leave the matched state as soon as we see the next Pop
						ms.extraDepth += 1
						Signal.Continue

					case None =>
						// entering a new context, time to start a new transformer
						val nextTransformer =
							try getTransformer(push)
							catch { case NonFatal(e) => throw SpacException.addTrace(e, trace.asSpacTraceElems) }
						val newInnerHandler = Transformer.Handler.bindDownstream(nextTransformer.newHandler, innerTransformerOut)
						state = Some(MatchedState(trace, Some(newInnerHandler), 0))
						Signal.Continue
				}

			case Left(ContextPop) =>
				state match {
					case Some(MatchedState(trace, innerOpt, 0)) =>
						// popping out of our matched state; inner transformer needs to be finished if it hasn't already
						state = None
						innerOpt match {
							case None => Signal.Continue
							case Some(inner) =>
								try inner.finish()
								catch { case NonFatal(e) => throw SpacException.addTrace(e, trace.asSpacTraceElems) }
								out.pushMany(innerTransformerOut.take().iterator)
						}

					case Some(ms) =>
						// a pop corresponding to an extra push. In practice this shouldn't happen
						ms.extraDepth -= 1
						Signal.Continue

					case None =>
						// a pop while we're not in any matched state can be ignored... in practice it won't happen
						Signal.Continue
				}
		}

		def finish(out: Transformer.HandlerWrite[Out]) = state match {
			case None =>
				// clean exit, no extra actions taken
				()

			case Some(MatchedState(_, None, _)) =>
				// in a match, but the inner transformer had finished, so no extra actions taken
				()

			case Some(MatchedState(trace, Some(inner), _)) =>
				// in a matched context with an inner transformer left unfinished
				try inner.finish()
				catch { case NonFatal(e) => throw SpacException.addTrace(e, trace.asSpacTraceElems) }
				out.pushMany(innerTransformerOut.take().iterator)
		}
	}
}