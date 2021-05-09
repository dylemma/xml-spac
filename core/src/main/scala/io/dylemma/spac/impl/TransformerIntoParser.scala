package io.dylemma.spac
package impl

class TransformerIntoParser[In, X, Out](t: Transformer[In, X], p: Parser[X, Out]) extends Parser[In, Out] {
	def newHandler = new Parser.Handler[In, Out] {
		val downstream = new TransformerIntoParser.ParserProxy(p)
		val h = Transformer.Handler.bindDownstream(t.newHandler, downstream)
		def step(in: In): Either[Out, Parser.Handler[In, Out]] = {
			val signal = h.push(in)
			if (signal.isStop) {
				downstream.checkResult match {
					case Some(out) => Left(out)
					case None => Right(this)
				}
			} else {
				Right(this)
			}
		}
		def finish() = {
			h.finish()
			downstream.checkResult.get
		}
	}
	override def toString = s"$t into $p"
}

object TransformerIntoParser {

	class ParserProxy[In, Out](parser: Parser[In, Out]) extends Transformer.BoundHandler[In] {
		private var current = parser.newHandler
		private var result: Option[Out] = None
		def finish(): Unit = {
			if (result.isEmpty) {
				result = Some(current.finish())
			}
		}
		def push(in: In): Signal = {
			if (result.isEmpty) {
				current.step(in) match {
					case Left(out) =>
						result = Some(out)
						Signal.Stop
					case Right(cont) =>
						current = cont
						Signal.Continue
				}
			} else {
				Signal.Stop
			}
		}
		def checkResult: Option[Out] = result
	}

}