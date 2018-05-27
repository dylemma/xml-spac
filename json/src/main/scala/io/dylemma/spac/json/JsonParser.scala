package io.dylemma.spac.json

import io.dylemma.spac.handlers.FallbackSetHandler
import io.dylemma.spac.json.handlers.PrimitiveValueHandler
import io.dylemma.spac.{FromHandlerFactory, HandlerFactory, ParserCompanion, ParserLike, Splitter}

abstract class JsonParser[+A] extends ParserLike[JsonEvent, JsonStackElem, A, JsonParser] {
	def nullable: JsonParser[Option[A]] = JsonParser.nullable(this)
}

object JsonParser extends ParserCompanion[JsonEvent, JsonParser] {

	val handlerFactoryConverter: FromHandlerFactory[JsonEvent, JsonParser] = new FromHandlerFactory[JsonEvent, JsonParser] {
		def makeInstance[Out](hf: HandlerFactory[JsonEvent, Out], debugName: String): JsonParser[Out] = new JsonParser[Out] {
			def makeHandler() = hf.makeHandler()
			override def toString = debugName
		}
	}

	def forPrimitive[A](describePrimitive: String, matchPrimitive: PartialFunction[JsonValueEvent, A]): JsonParser[A] = new JsonParser[A] {
		override def toString = s"Parser.forPrimitive[$describePrimitive]"
		def makeHandler() = new PrimitiveValueHandler(describePrimitive, matchPrimitive)
	}

	implicit lazy val forString: JsonParser[String] = forPrimitive("String", { case JsonEvent.JString(s) => s })
	implicit lazy val forInt: JsonParser[Int] = forPrimitive("Int", { case JsonEvent.JLong(num) => num.intValue })
	implicit lazy val forLong: JsonParser[Long] = forPrimitive("Long", { case JsonEvent.JLong(num) => num })
	implicit lazy val forFloat: JsonParser[Float] = forPrimitive("Float", { case JsonEvent.JDouble(num) => num.floatValue })
	implicit lazy val forDouble: JsonParser[Double] = forPrimitive("Double", { case JsonEvent.JDouble(num) => num })
	implicit lazy val forBoolean: JsonParser[Boolean] = forPrimitive("Boolean", { case JsonEvent.JBool(bool) => bool })
	implicit lazy val forNull: JsonParser[None.type] = forPrimitive("Null", { case JsonEvent.JNull => None })

	def apply[T](implicit parser: JsonParser[T]): JsonParser[T] = parser
	def nullable[T](implicit parser: JsonParser[T]): JsonParser[Option[T]] = oneOf(parser.map(Some(_)), forNull)

	def listOf[T](implicit parser: JsonParser[T]): JsonParser[List[T]] = Splitter(anyIndex).asListOf(parser)
		.expectInputs(List("a '[' token" -> { _ == JsonEvent.ArrayStart }))
   	.withName(s"Parser.listOf($parser)")
}
