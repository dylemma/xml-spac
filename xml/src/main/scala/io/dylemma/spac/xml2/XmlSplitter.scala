package io.dylemma.spac.xml2

import cats.Monad
import io.dylemma.spac.{ContextMatchSplitter, ContextMatcher}
import io.dylemma.spac.xml2.XmlEvent.ElemStart

object XmlSplitter {
	def apply[F[+_]: Monad, Context](matcher: ContextMatcher[ElemStart, Context]) = {
		new ContextMatchSplitter[F, XmlEvent, ElemStart, Context](matcher)
	}
}
