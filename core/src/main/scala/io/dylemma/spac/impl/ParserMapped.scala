package io.dylemma.spac
package impl

class ParserMapped[In, A, B](self: Parser[In, A], f: A => B) extends Parser[In, B] {
	def newHandler = new ParserMapped.Handler(self.newHandler, f)

	override def map[Out2](g: B => Out2) = new ParserMapped[In, A, Out2](self, a => g(f(a)))
}

object ParserMapped {
	class Handler[In, Out, Out2](
		private var inner: Parser.Handler[In, Out],
		f: Out => Out2
	) extends Parser.Handler[In, Out2]
	{
		def step(in: In) = inner.step(in) match {
			case Left(out) => Left(f(out))
			case Right(cont) =>
				inner = cont
				Right(this)
		}
		def finish() = f(inner.finish())
	}
}