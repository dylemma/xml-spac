package io.dylemma.spac
package impl

class SplitterByInputMatch[In, C](pf: PartialFunction[In, C]) extends Splitter[In, C] {
	val addBoundaries = new SplitterByInputMatch.Boundaries(pf)
}
object SplitterByInputMatch {
	class Boundaries[In, C](pf: PartialFunction[In, C]) extends Transformer[In, Either[ContextChange[In, C], In]] {
		def newHandler = new SplitterByInputMatch.Handler(pf, false)
	}

	class Handler[In, C](
		pf: PartialFunction[In, C],
		private var startedMatching: Boolean
	) extends Transformer.Handler[In, Either[ContextChange[In, C], In]] {
		def step(in: In) = {
			pf.lift(in) match {
				case None =>
					// nothing special going on - just re-emit the input
					Emit.one(Right(in)) -> Some(this)

				case Some(newContext) =>
					val popC = if (startedMatching) Emit.one(Left(ContextPop)) else Emit.nil
					val push = Left(ContextPush(ContextTrace.empty, newContext))
					val emit = popC :+ push :+ Right(in)
					startedMatching = true
					emit -> Some(this)
			}
		}

		def finish() = {
			if (startedMatching) Emit.one(Left(ContextPop))
			else Emit.nil
		}
	}
}
