package io.dylemma.spac.json

import io.dylemma.spac.Parser
import io.dylemma.spac.json.handlers.PrimitiveValueHandler

object JsonParser {

	def forPrimitive[A](describePrimitive: String, matchPrimitive: PartialFunction[JsonValueEvent, A]): JsonParser[A] = new JsonParser[A] {
		override def toString = s"Parser.forPrimitive[$describePrimitive]"
		def makeHandler() = new PrimitiveValueHandler(describePrimitive, matchPrimitive)
	}

	lazy val forString: JsonParser[String] = forPrimitive("String", { case JsonEvent.JString(s) => s })
	lazy val forInt: JsonParser[Int] = forPrimitive("Int", { case JsonEvent.JLong(num) => num.intValue })
	lazy val forLong: JsonParser[Long] = forPrimitive("Long", { case JsonEvent.JLong(num) => num })
	lazy val forFloat: JsonParser[Float] = forPrimitive("Float", { case JsonEvent.JDouble(num) => num.floatValue })
	lazy val forDouble: JsonParser[Double] = forPrimitive("Double", { case JsonEvent.JDouble(num) => num })
	lazy val forBoolean: JsonParser[Boolean] = forPrimitive("Boolean", { case JsonEvent.JBool(bool) => bool })
	lazy val forNull: JsonParser[None.type] = forPrimitive("Null", { case JsonEvent.JNull => None })

	/** Implicitly resolve a `JsonParser[T]`.
	  *
	  * Note that because the "class" `JsonParser` is only a type alias, the `JsonParser` object
	  * unfortunately doesn't behave as a companion object. To work around this, the various `forXYZ`
	  * parser methods are redefined as implicits in the `io.dylemma.spac.json.syntax.Implicits` trait,
	  * which is included in the `io.dylemma.spac.json` package object.
	  *
	  * @param parser An implicitly-available JsonParser[T] which will be returned
	  * @tparam T
	  * @return `parser`
	  */
	def apply[T](implicit parser: JsonParser[T]): JsonParser[T] = parser
	def nullable[T](implicit parser: JsonParser[T]): JsonParser[Option[T]] = Parser.oneOf(parser.map(Some(_)), forNull)

	def listOf[T](implicit parser: JsonParser[T]): JsonParser[List[T]] = JsonSplitter(anyIndex).asListOf(parser)
		.expectInputs[JsonEvent](List("a '[' token" -> { _ == JsonEvent.ArrayStart }))
   	.withName(s"Parser.listOf($parser)")
}

