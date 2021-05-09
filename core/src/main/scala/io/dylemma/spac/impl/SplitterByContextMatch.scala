package io.dylemma.spac
package impl

import cats.data.Chain

import scala.collection.mutable

case class SplitterByContextMatch[In, Elem, C](matcher: ContextMatcher[Elem, C], matcherPos: CallerPos)(implicit S: StackLike[In, Elem]) extends Splitter[In, C] {
	val addBoundaries = Transformer
		.spacFrame(SpacTraceElement.InSplitter(matcher.toString, matcherPos))
		.through(S.interpret)
		.through(new SplitterByContextMatch.Boundaries(matcher))
}

object SplitterByContextMatch {

	class Boundaries[In, Elem, C](matcher: ContextMatcher[Elem, C]) extends Transformer[Either[ContextChange[In, Elem], In], Either[ContextChange[In, C], In]] {
		override def toString = s"SplitterBoundaries($matcher)"
		def newHandler = new SplitterByContextMatch.Handler(matcher)
	}

	/** Splitter implementation detail that re-interprets stack states into contexts.
	  * As stack elements are pushed and popped via the incoming stream,
	  * it plugs the accumulated stack into the given `matcher` to determine if a
	  * matched context has been entered (or exited). The matched context is
	  * communicated as `ContextChange` events of the `C` type via the outgoing stream.
	  */
	class Handler[In, Elem, C](matcher: ContextMatcher[Elem, C]) extends Transformer.Handler[Either[ContextChange[In, Elem], In], Either[ContextChange[In, C], In]] {
		override def toString = s"Splitter($matcher)"

		private val traces = new mutable.ArrayBuffer[ContextTrace[In]]
		private val stack = new mutable.ArrayBuffer[Elem]
		private var isMatched = false
		private var extraDepth = 0

		def finish(out: Transformer.HandlerWrite[Either[ContextChange[In, C], In]]): Unit = {
			// if we're in a matched context, it needs to be closed (via ContextPop) before we finish
			if (isMatched) out.push(Left(ContextPop))
		}
		def push(in: Either[ContextChange[In, Elem], In], out: Transformer.HandlerWrite[Either[ContextChange[In, C], In]]): Signal = in match {
			case Right(in) =>
				// just pass the event along, with no change in state
				out.push(Right(in))

			case Left(ContextPush(incomingTrace, elem)) =>
				if (isMatched) {
					// already in a matched context, so the push simply increments our extraDepth
					extraDepth += 1
					Signal.Continue
				} else {
					// the push may be enough to put us into a matching state; we need to check the `matcher`
					traces += incomingTrace
					stack += elem

					matcher(stack, 0, stack.length) match {
						case None =>
							// no new context match, just continue
							Signal.Continue
						case Some(c) =>
							// new matched context!
							isMatched = true
							extraDepth = 0
							val combinedTrace = traces.foldLeft(ContextTrace[In](Chain.empty))(_ / _)
							val change = ContextPush(combinedTrace, c)
							out.push(Left(change))
					}
				}

			case Left(ContextPop) =>
				if (isMatched) {
					if (extraDepth == 0) {
						// matched context has ended; pop from the stack and traces, and emit the pop event
						traces.remove(traces.length - 1)
						stack.remove(stack.length - 1)
						isMatched = false
						out.push(Left(ContextPop))
					} else {
						// still in the matched context, minus one extra depth
						extraDepth -= 1
						Signal.Continue
					}
				} else {
					// just pop the last values off of the stack/traces
					traces.remove(traces.length - 1)
					stack.remove(stack.length - 1)
					Signal.Continue
				}
		}
	}
}

