package io.dylemma.spac
package impl

class ParserDefer[-In, +Out](makeInner: () => Parser[In, Out]) extends Parser[In, Out] {
	def newHandler = makeInner().newHandler
}
