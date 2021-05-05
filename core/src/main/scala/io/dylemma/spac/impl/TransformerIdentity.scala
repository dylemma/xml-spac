package io.dylemma.spac
package impl

class TransformerIdentity[In] extends Transformer.Stateless[In, In] {
	def push(in: In, out: Transformer.HandlerWrite[In]): Signal = out.push(in)
	def finish(out: Transformer.HandlerWrite[In]): Unit = ()

	override def through[Out2](next: Transformer[In, Out2]) = next

	override def toString = "TransformerIdentity"
}
