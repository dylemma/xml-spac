package io.dylemma.spac
package impl

class TransformerWithName[In, Out](self: Transformer[In, Out], name: String) extends Transformer[In, Out] {
	def newHandler = self.newHandler
	override def toString = name
}
