package io.dylemma.spac

sealed trait ContextLineMatcher {
	def unapply(loc: ContextLocation): Boolean
}
object ContextLineMatcher {
	def apply(expectedLine: Long)(implicit support: ContextLineNumberSupport): ContextLineMatcher = {
		if (support.canCheckLineNumbers) {
			new ContextLineMatcher {
				def unapply(loc: ContextLocation): Boolean = loc.get(ContextLocation.Tag.LineNumber).fold(false)(_ == expectedLine)
			}
		} else {
			new ContextLineMatcher {
				def unapply(loc: ContextLocation): Boolean = true
			}
		}
	}
}
