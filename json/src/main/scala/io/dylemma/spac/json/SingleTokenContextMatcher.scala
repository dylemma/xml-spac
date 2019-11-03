package io.dylemma.spac.json

import io.dylemma.spac.ContextMatcher

import scala.collection.IndexedSeq

trait SingleTokenContextMatcher[A] extends ContextMatcher[JsonStackElem, A]{
	def applyToken(token: JsonStackElem): Option[A]

	def applyChained[B](stack: IndexedSeq[JsonStackElem], offset: Int, avail: Int, next: ContextMatcher[JsonStackElem, B]): Option[(A, B)] = {
		if(avail >= 1){
			for {
				a <- applyToken(stack(offset))
				b <- next(stack, offset + 1, avail - 1)
			} yield a -> b
		} else {
			None
		}
	}
}

object SingleTokenContextMatcher {
	def apply[A](f: JsonStackElem => Option[A], debugName: String): SingleTokenContextMatcher[A] = new SingleTokenContextMatcher[A] {
		override def toString = debugName
		def applyToken(token: JsonStackElem) = f(token)
	}
}