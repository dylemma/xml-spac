package io.dylemma.spac
package impl

class ParserAsTransformer[In, Out](self: Parser[In, Out]) extends Transformer[In, Out] {
	def newHandler = new ParserAsTransformer.Handler(self.newHandler)
}

object ParserAsTransformer {
	class Handler[In, Out](var self: Parser.Handler[In, Out]) extends Transformer.Handler[In, Out] {
		override def toString = s"($self).asTransformer"

		def push(in: In, out: Transformer.HandlerWrite[Out]): Signal = self.step(in) match {
			case Right(cont) =>
				self = cont
				Signal.Continue
			case Left(result) =>
				out.push(result)
				Signal.Stop
		}

		def finish(out: Transformer.HandlerWrite[Out]): Unit = {
			out.push(self.finish())
		}
	}
}