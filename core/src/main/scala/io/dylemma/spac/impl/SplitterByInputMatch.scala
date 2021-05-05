package io.dylemma.spac
package impl

class SplitterByInputMatch[In, C](pf: PartialFunction[In, C]) extends Splitter[In, C] {
	val addBoundaries = new SplitterByInputMatch.Boundaries(pf)
}
object SplitterByInputMatch {
	class Boundaries[In, C](pf: PartialFunction[In, C]) extends Transformer[In, Either[ContextChange[In, C], In]] {
		def newHandler = new BounariesHandler(pf)
	}

	class BounariesHandler[In, C](pf: PartialFunction[In, C]) extends Transformer.Handler[In, Either[ContextChange[In, C], In]] {
		private var startedMatching = false

		def push(in: In, out: Transformer.HandlerWrite[Either[ContextChange[In, C], In]]): Signal = {
			if (pf.isDefinedAt(in)) {
				startedMatching = true
				val ctx = pf(in)
				val s1 = if (startedMatching) out.push(Left(ContextPop)) else Signal.Continue
				s1 ||
					out.push(Left(ContextPush(ContextTrace.empty, ctx))) ||
					out.push(Right(in))
			} else {
				// nothing special going on - just re-emit the input
				out.push(Right(in))
			}
		}

		def finish(out: Transformer.HandlerWrite[Either[ContextChange[In, C], In]]): Unit = {
			if (startedMatching) out.push(Left(ContextPop))
		}
	}
}
