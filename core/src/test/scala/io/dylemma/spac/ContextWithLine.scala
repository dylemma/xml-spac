package io.dylemma.spac

object ContextWithLine {
	def unapply(loc: ContextLocation) = loc.get(ContextLocation.Tag.LineNumber)
}
