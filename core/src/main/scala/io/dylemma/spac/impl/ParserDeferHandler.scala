package io.dylemma.spac
package impl

class ParserDeferHandler[In, Out](makeHandler: () => Parser.Handler[In, Out]) extends Parser[In, Out] {
	def newHandler = makeHandler()
}
