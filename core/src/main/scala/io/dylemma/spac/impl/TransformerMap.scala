package io.dylemma.spac
package impl

case class TransformerMap[A, B](f: A => B) extends Transformer.Stateless[A, B]{
	def push(in: A, out: Transformer.HandlerWrite[B]): Signal = out.push(f(in))
	def finish(out: Transformer.HandlerWrite[B]): Unit = ()
}
