package io.dylemma.spac.json

import io.dylemma.spac._

class JsonSplitter[+Context](matcher: ContextMatcher[JsonStackElem, Context]) extends ContextStackSplitter[JsonEvent, JsonStackElem, Context](matcher) {
	object asListOf {
		def choose[Out](implicit parser: Context => Parser[JsonEvent, Out]) = as(parser).parseToList
		def apply[Out: JsonParser] = as[Out].parseToList
	}

	object first {
		def choose[Out](implicit parser: Context => Parser[JsonEvent, Out]) = as(parser).parseFirst
		def apply[Out: JsonParser] = as[Out].parseFirst
	}

	object firstOption {
		def choose[Out](implicit parser: Context => Parser[JsonEvent, Out]) = as(parser).parseFirstOption
		def apply[Out: JsonParser] = as[Out].parseFirstOption
	}

	object firstNotNull {
		def choose[Out](implicit parser: Context => JsonParser[Out]) = {
			as(ctx => JsonParser.nullable(parser(ctx))).collect { case Some(out) => out }.parseFirst
		}
		def apply[Out: JsonParser] = {
			as(ctx => JsonParser.nullable[Out]).collect { case Some(out) => out }.parseFirst
		}
	}
}

object JsonSplitter {

	/** Create a JsonSplitter using the given `matcher` to determine where sub-streams start and end.
	  * For example, `JsonSplitter("foo")`, when applied to the json:
	  * {{{
	  * {
	  *   "foo": [1, 2],
	  *   "bar": true
	  * }
	  * }}}
	  * would identify the value of the object's "foo" field as a sub-stream of JsonEvents, containing
	  * the events `ArrayStart, IndexStart(0), JLong(1), IndexEnd, IndexStart(1), JLong(2), IndexEnd, ArrayEnd`.
	  *
	  * Any context matched by the `matcher` will be passed through the `joiner` functions if you
	  * call `as`, `map`, or `flatMap` on the resulting splitter, and thus the matched context
	  * can be used to decide how you parse each sub-stream.
	  *
	  * @param matcher A ContextMatcher used to identify where each sub-stream begins and ends,
	  *                and extracts some context value to identify each sub-stream.
	  * @tparam Context The type of the "context" matched by the `matcher`
	  * @return A new JsonSplitter that will split a stream into sub-streams identified by the `matcher`
	  */
	def apply[Context](matcher: ContextMatcher[JsonStackElem, Context]) = new JsonSplitter(matcher)
}