package io.dylemma.spac
package impl

import cats.data.Chain

case class TransformerSpacFrame[A](elems: Chain[SpacTraceElement]) extends Transformer.Stateless[A, A] {
	def push(in: A, out: Transformer.HandlerWrite[A]): Signal = out.push(in)
	def finish(out: Transformer.HandlerWrite[A]): Unit = ()
	override def bubbleUp(err: Throwable): Nothing = throw SpacException.addTrace(err, elems)
}

