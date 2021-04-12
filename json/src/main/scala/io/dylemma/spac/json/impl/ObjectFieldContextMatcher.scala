package io.dylemma.spac
package json
package impl

class ObjectFieldContextMatcher[A](debugName: String, f: JsonEvent.FieldStart => Option[A]) extends ContextMatcher[JsonStackElem, A] {
	override def toString = debugName
	def applyChained[B](stack: collection.IndexedSeq[JsonStackElem], offset: Int, avail: Int, next: ContextMatcher[JsonStackElem, B]) = {
		if (avail >= 2) {
			for {
				fieldStart <- stack(offset + 1).asFieldStart
				if stack(offset).isObjectStart
				a <- f(fieldStart)
				b <- next(stack, offset + 2, avail - 2)
			} yield (a, b)
		} else {
			None
		}
	}
}
