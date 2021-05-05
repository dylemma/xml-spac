package io.dylemma.spac
package impl

case class TransformerCollect[In, Out](f: PartialFunction[In, Out]) extends Transformer.Stateless[In, Out] {
	def push(in: In, out: Transformer.HandlerWrite[Out]): Signal = {
		if (f.isDefinedAt(in)) out.push(f(in))
		else Signal.Continue
	}
	def finish(out: Transformer.HandlerWrite[Out]): Unit = ()
}
