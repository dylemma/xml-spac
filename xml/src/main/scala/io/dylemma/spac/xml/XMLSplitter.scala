package io.dylemma.spac.xml

import io.dylemma.spac._
import javax.xml.namespace.QName
import javax.xml.stream.events.{StartElement, XMLEvent}

import scala.util.Try

class XMLSplitter[+Context](matcher: XMLContextMatcher[Context]) extends ContextStackSplitter[XMLEvent, StartElement, Context](matcher) { self =>

	def attr(name: QName): Transformer[XMLEvent, String] = map(XMLParser.forMandatoryAttribute(name))
	def attr(name: String): Transformer[XMLEvent, String] = map(XMLParser.forMandatoryAttribute(name))
	def asText: Transformer[XMLEvent, String] = map(XMLParser.forText)

	def asListOf[Out](implicit parser: Context => HandlerFactory[XMLEvent, Out]): XMLParser[List[Out]] = as[Out].parseToList

	object first {
		def apply[Out](implicit parser: Context => HandlerFactory[XMLEvent, Out]) = map(parser).parseFirst
		def attr(name: QName) = map(XMLParser.forMandatoryAttribute(name)).parseFirst
		def attr(name: String) = map(XMLParser.forMandatoryAttribute(name)).parseFirst
		def asText = map(XMLParser.forText).parseFirst
	}

	object firstOption {
		def apply[Out](implicit parser: Context => HandlerFactory[XMLEvent, Try[Out]]) = map(parser).parseFirstOption
		def attr(name: QName) = map(XMLParser.forMandatoryAttribute(name)).parseFirstOption
		def attr(name: String) = map(XMLParser.forMandatoryAttribute(name)).parseFirstOption
		def asText = self.map(XMLParser.forText).parseFirstOption
	}

}

object XMLSplitter {
	/** Create an XMLSplitter using the given `matcher` to determine where sub-streams start and end.
	  * For example, `XMLSplitter(* \ "foo")`, when applied to the xml:
	  * {{{
	  * <elem>
	  *    <foo>hello</foo>
	  *    <foo>goodbye</foo>
	  * </elem>
	  * }}}
	  * would identify the first and second `<foo>` elements as separate substreams, each containing
	  * the events `StartElement("foo"), Text("hello"), EndElement("foo")`, and
	  * `StartElement("foo"), Text("goodbye"), EndElement("foo")` respectively.
	  *
	  * Any context matched by the `matcher` will be passed through the `joiner` functions if you
	  * call `as`, `map`, or `flatMap` on the resulting splitter, and thus the matched context
	  * can be used to decide how you parse each sub-stream.
	  *
	  * @param matcher A ContextMatcher used to identify where each sub-stream begins and ends,
	  *                and extracts some context value to identify each sub-stream.
	  * @tparam Context The type of the "context" matched by the `matcher`
	  * @return A new XMLSplitter that will split a stream into sub-streams identified by the `matcher`
	  */
	def apply[Context](matcher: XMLContextMatcher[Context]) = new XMLSplitter(matcher)
}