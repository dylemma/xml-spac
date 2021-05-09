package io.dylemma.spac
package impl

case class TransformerFilter[In](f: In => Boolean) extends Transformer.Stateless[In, In] {
	def push(in: In, out: Transformer.HandlerWrite[In]): Signal = {
		if (f(in)) out.push(in)
		else Signal.Continue
	}
	def finish(out: Transformer.HandlerWrite[In]): Unit = ()
}
