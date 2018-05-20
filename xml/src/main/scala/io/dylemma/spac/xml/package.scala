package io.dylemma.spac

import javax.xml.stream.events.{StartElement, XMLEvent}

import io.dylemma.spac.xml.syntax.{ContextMatcherSyntax, TransformerSyntax}

package object xml
	extends ContextMatcherSyntax
	with TransformerSyntax
{
	object ContextMatcherSyntax extends ContextMatcherSyntax
	object TransformerSyntax extends TransformerSyntax

	implicit def consumableLikeXMLEvents: ConsumableLike[XMLEvents, XMLEvent] = XMLEvents.consumableLike
	implicit def consumableLikeXMLResource[T: XMLResource]: ConsumableLike[T, XMLEvent] = XMLResource.consumableLike[T]
	implicit def xmlSplitterApply: SplitterApply[StartElement, XMLSplitter] = XMLSplitter.xmlSplitterApply

	type Parser[+A] = XMLParser[A]
	val Parser = XMLParser
}
