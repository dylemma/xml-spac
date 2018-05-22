package io.dylemma.spac.json

import io.dylemma.spac._

class JsonSplitter[+Context](matcher: ContextMatcher[JsonStackElem, Context]) extends BaseStackSplitter[JsonEvent, JsonStackElem, Context](matcher) {
	// TODO: add JSON-specific parsing conveniences here

	def asListOf[Out](implicit parser: Context => HandlerFactory[JsonEvent, Out]) = as[Out].parseToList

	object first {
		def apply[Out](implicit parser: Context => HandlerFactory[JsonEvent, Out]) = as(parser).parseFirst
	}

	object firstOption {
		def apply[Out](implicit parser: Context => HandlerFactory[JsonEvent, Out]) = as(parser).parseFirstOption
	}
}

object JsonSplitter extends SplitterApply[JsonStackElem, JsonSplitter] {
	def apply[Context](matcher: ContextMatcher[JsonStackElem, Context]) = new JsonSplitter(matcher)
}