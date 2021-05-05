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
		def push(in: In, out: Transformer.HandlerWrite[Either[ContextChange[In, C], In]]): Signal = matcher(in) match {
			case Some(newContext) if !isMatching =>
				out.push(Left(ContextPush(ContextTrace.empty, newContext))) || out.push(Right(in))
			case None if isMatching =>
				out.push(Left(ContextPop)) || out.push(Right(in))
			case _ =>
				out.push(Right(in))
		}

		def finish(out: Transformer.HandlerWrite[Either[ContextChange[In, C], In]]): Unit = {
			if (isMatching) out.push(Left(ContextPop))
		}
	}
}
