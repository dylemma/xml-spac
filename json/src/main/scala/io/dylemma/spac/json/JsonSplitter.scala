package io.dylemma.spac.json

import io.dylemma.spac._

class JsonSplitter[+Context](matcher: ContextMatcher[JsonStackElem, Context]) extends BaseStackSplitter[JsonEvent, JsonStackElem, Context](matcher) {
	// TODO: add JSON-specific parsing conveniences here

	object asListOf {
		def choose[Out](implicit parser: Context => HandlerFactory[JsonEvent, Out]) = as[Out].parseToList
		def apply[Out: Parser] = as[Out].parseToList
	}

	object first {
		def choose[Out](implicit parser: Context => HandlerFactory[JsonEvent, Out]) = as(parser).parseFirst
		def apply[Out: Parser] = as[Out].parseFirst
	}

	object firstOption {
		def choose[Out](implicit parser: Context => HandlerFactory[JsonEvent, Out]) = as(parser).parseFirstOption
		def apply[Out: Parser] = as[Out].parseFirstOption
	}
}

object JsonSplitter extends SplitterApply[JsonStackElem, JsonSplitter] {
	def apply[Context](matcher: ContextMatcher[JsonStackElem, Context]) = new JsonSplitter(matcher)
}