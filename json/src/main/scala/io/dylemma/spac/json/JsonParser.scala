package io.dylemma.spac.json

import io.dylemma.spac.{FromHandlerFactory, HandlerFactory, ParserCompanion, ParserLike}

// TODO: ParserLike wants a Stackable, but JSON can't fit that interface.
// TODO: We probably want to replace Stackable with the ContextTracker and reimplement the followedBy logic.
abstract class JsonParser[+A] extends ParserLike[JsonEvent, A, JsonParser]()(implicitly, ???)

object JsonParser extends ParserCompanion[JsonEvent, JsonParser] {

	implicit val handlerFactoryConverter: FromHandlerFactory[JsonEvent, JsonParser] = new FromHandlerFactory[JsonEvent, JsonParser] {
		def makeInstance[Out](hf: HandlerFactory[JsonEvent, Out], debugName: String): JsonParser[Out] = new JsonParser[Out] {
			def makeHandler() = hf.makeHandler()
			override def toString = debugName
		}
	}
}
