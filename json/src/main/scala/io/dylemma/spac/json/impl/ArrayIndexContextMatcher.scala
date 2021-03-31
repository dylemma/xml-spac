package io.dylemma.spac
package json
package impl

class ArrayIndexContextMatcher[A](debugName: String, f: JsonEvent.IndexStart => Option[A]) extends ContextMatcher[JsonStackElem, A] {
	override def toString = debugName
	def applyChained[B](stack: collection.IndexedSeq[JsonStackElem], offset: Int, avail: Int, next: ContextMatcher[JsonStackElem, B]) = {
		if (avail >= 2) {
			for {
				indexStart <- stack(offset + 1).asIndexStart
				if stack(offset).isArrayStart
				a <- f(indexStart)
				b <- next(stack, offset + 2, avail - 2)
			} yield (a, b)
		} else {
			None
		}
	}
}
