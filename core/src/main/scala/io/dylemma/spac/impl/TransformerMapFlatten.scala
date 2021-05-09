package io.dylemma.spac
package impl

case class TransformerMapFlatten[In, Out](f: In => Iterable[Out]) extends Transformer.Stateless[In, Out] {
	def push(in: In, out: Transformer.HandlerWrite[Out]): Signal = {
		out.pushMany(f(in).iterator)
	}

	def finish(out: Transformer.HandlerWrite[Out]): Unit = ()
}
