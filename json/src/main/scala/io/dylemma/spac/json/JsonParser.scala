package io.dylemma.spac.json

import io.dylemma.spac.{FromHandlerFactory, HandlerFactory, ParserCompanion, ParserLike}

abstract class JsonParser[+A] extends ParserLike[JsonEvent, A, JsonParser]

object JsonParser extends ParserCompanion[JsonEvent, JsonParser] {

	implicit val handlerFactoryConverter: FromHandlerFactory[JsonEvent, JsonParser] = new FromHandlerFactory[JsonEvent, JsonParser] {
		def makeInstance[Out](hf: HandlerFactory[JsonEvent, Out], debugName: String): JsonParser[Out] = new JsonParser[Out] {
			def makeHandler() = hf.makeHandler()
			override def toString = debugName
		}
	}
}
