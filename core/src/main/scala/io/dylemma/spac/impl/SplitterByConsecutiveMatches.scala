package io.dylemma.spac
package impl

class SplitterByConsecutiveMatches[In, C](matcher: PartialFunction[In, C]) extends Splitter[In, C] {
	val addBoundaries = new SplitterByConsecutiveMatches.Boundaries(matcher)
}
object SplitterByConsecutiveMatches {
	class Boundaries[In, C](matcher: PartialFunction[In, C]) extends Transformer[In, Either[ContextChange[In, C], In]] {
		def newHandler = new BoundariesHandler(matcher.lift, false)
	}

	class BoundariesHandler[In, C](matcher: In => Option[C], private var isMatching: Boolean) extends Transformer.Handler[In, Either[ContextChange[In, C], In]] {
		// note: as long as the matcher keeps returning `Some`, it doesn't matter if the `C` value changes; this handler keeps the current context
		def step(in: In) = {
			matcher(in) match {
				case Some(newContext) if !isMatching =>
					// only switch contexts if we were not already in a matched state
					isMatching = true
					Emit(Left(ContextPush(ContextTrace.empty, newContext)), Right(in)) -> Some(this)
				case None if isMatching =>
					isMatching = false
					// leaving a matched state
					Emit(Left(ContextPop), Right(in)) -> Some(this)
				case _ =>
					// no context change; just emit the input
					Emit(Right(in)) -> Some(this)
			}
		}

		def finish() = {
			// exit the current context, if it's in one
			if (isMatching) Emit.one(Left(ContextPop)) else Emit.nil
		}
	}
}
