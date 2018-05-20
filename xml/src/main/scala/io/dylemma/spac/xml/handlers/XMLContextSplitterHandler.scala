package io.dylemma.spac.xml.handlers

import javax.xml.stream.events.{StartElement, XMLEvent}

import io.dylemma.spac.{ContextMatcher, Handler, HandlerFactory}
import io.dylemma.spac.core.Format
import io.dylemma.spac.handlers.StackFormatSplitterHandler

class XMLContextSplitterHandler[Context, P, Out](
	matcher: ContextMatcher[StartElement, Context],
	joiner: Context => HandlerFactory[XMLEvent, P],
	downstream: Handler[P, Out]
) extends StackFormatSplitterHandler(
	Format.forXml,
	matcher,
	joiner,
	downstream
)