package io.dylemma.spac
package impl

class ParserAsTransformer[In, Out](self: Parser[In, Out]) extends Transformer[In, Out] {
	def newHandler = new ParserAsTransformer.Handler(self.newHandler)
}

object ParserAsTransformer {
	class Handler[In, Out](var self: Parser.Handler[In, Out]) extends Transformer.Handler[In, Out] {
		override def toString = s"($self).asTransformer"
		def step(in: In) = self.step(in) match {
			case Right(cont) =>
				self = cont
				Emit.nil -> Some(this)
			case Left(out) =>
				Emit.one(out) -> None
		}
		def finish() = {
			Emit.one { self.finish() }
		}
	}
}