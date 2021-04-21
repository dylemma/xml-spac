package io.dylemma.spac

import io.dylemma.spac.json.impl.{ArrayIndexContextMatcher, JsonParserTypedFirst, ObjectFieldContextMatcher}
import org.tpolecat.typename.TypeName

import scala.language.implicitConversions

package object json {

	val JsonParser: ParserApplyWithBoundInput[JsonEvent] = Parser[JsonEvent]

	type JsonParser[+Out] = Parser[JsonEvent, Out]

	implicit class JsonParserApplyOps(val parserApply: ParserApplyWithBoundInput[JsonEvent]) extends AnyVal {
		def forPrimitive[A](describePrimitive: String, matchPrimitive: JsonEvent => Option[A]): JsonParser[A] = new JsonParserTypedFirst(describePrimitive, matchPrimitive)

		def nullable[T](implicit parser: JsonParser[T]): JsonParser[Option[T]] = JsonParser.oneOf(parser.map(Some(_)), forNull)

		def forString: JsonParser[String] = jsonParserForPrimitiveString
		def forInt: JsonParser[Int] = jsonParserForPrimitiveInt
		def forLong: JsonParser[Long] = jsonParserForPrimitiveLong
		def forFloat: JsonParser[Float] = jsonParserForPrimitiveFloat
		def forDouble: JsonParser[Double] = jsonParserForPrimitiveDouble
		def forBoolean: JsonParser[Boolean] = jsonParserForPrimitiveBoolean
		def forNull: JsonParser[None.type] = jsonParserForPrimitiveNull

		def fieldOf[T: TypeName : JsonParser](fieldName: String)(implicit callerPos: CallerPos): JsonParser[T] = fieldOf[T](fieldName, implicitly[JsonParser[T]])
		def fieldOf[T: TypeName](fieldName: String, parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[T] = Splitter.json(fieldName).joinBy(parser).parseFirst
			.expectInputs[JsonEvent](List("a '{' token" -> { _.isObjectStart }))
			.withName(s"JsonParser.fieldOf[${implicitly[TypeName[T]].value}]($fieldName)")

		def nullableFieldOf[T: TypeName: JsonParser](fieldName: String)(implicit callerPos: CallerPos): JsonParser[Option[T]] = nullableFieldOf[T](fieldName, implicitly[JsonParser[T]])
		def nullableFieldOf[T: TypeName](fieldName: String, parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[Option[T]] = Splitter.json(fieldName).joinBy(nullable(parser)).parseFirstOpt
			.map(_.flatten)
			.expectInputs[JsonEvent](List("a '{' token" -> { _.isObjectStart }))
			.withName(s"JsonParser.nullableFieldOf[${implicitly[TypeName[T]].value}]($fieldName)")

		def listOf[T: TypeName : JsonParser](implicit callerPos: CallerPos): JsonParser[List[T]] = listOf[T](implicitly[JsonParser[T]])
		def listOf[T: TypeName](parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[List[T]] = Splitter.json(anyIndex).joinBy(parser).parseToList
			.expectInputs[JsonEvent](List("a '[' token" -> { _.isArrayStart }))
			.withName(s"JsonParser.listOf[${ implicitly[TypeName[T]].value }]")

		def objectOf[T: TypeName : JsonParser](implicit callerPos: CallerPos): JsonParser[Map[String, T]] = objectOf[T](implicitly[JsonParser[T]])
		def objectOf[T: TypeName](parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[Map[String, T]] = Splitter.json(anyField)
			.map { field => parser.map(field -> _) }
			.parseToMap
			.expectInputs[JsonEvent](List("a '{' token" -> { _.isObjectStart }))
			.withName(s"JsonParser.objectOf[${ implicitly[TypeName[T]].value }]")

		def objectOfNullable[T: TypeName : JsonParser](implicit callerPos: CallerPos): JsonParser[Map[String, T]] = objectOfNullable[T](implicitly[JsonParser[T]])
		def objectOfNullable[T: TypeName](parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[Map[String, T]] = Splitter.json(anyField)
			.map { field => nullable(parser).map(field -> _) }
			.collect { case (field, Some(value)) => field -> value }
			.parseToMap
			.expectInputs[JsonEvent](List("a '{' token" -> { _.isObjectStart }))
			.withName(s"JsonParser.objectOfNullable[${ implicitly[TypeName[T]].value }]")
	}

	implicit val jsonParserForPrimitiveString: JsonParser[String] = JsonParser.forPrimitive("a String value", _.asString.map(_.stringValue))
	implicit val jsonParserForPrimitiveInt: JsonParser[Int] = JsonParser.forPrimitive("an Int value", _.asLong.map(_.longValue.intValue))
	implicit val jsonParserForPrimitiveLong: JsonParser[Long] = JsonParser.forPrimitive("a Long value", _.asLong.map(_.longValue))
	implicit val jsonParserForPrimitiveFloat: JsonParser[Float] = JsonParser.forPrimitive("a Float value", _.asDouble.map(_.doubleValue.floatValue))
	implicit val jsonParserForPrimitiveDouble: JsonParser[Double] = JsonParser.forPrimitive("a Double value", _.asDouble.map(_.doubleValue))
	implicit val jsonParserForPrimitiveBoolean: JsonParser[Boolean] = JsonParser.forPrimitive("a Boolean value", _.asBool.map(_.booleanValue))
	implicit val jsonParserForPrimitiveNull: JsonParser[None.type] = JsonParser.forPrimitive("a Null value", _.asNull.map(_ => None))

	val JsonSplitter: SplitterApplyWithBoundInput[JsonEvent] = Splitter[JsonEvent]

	type JsonSplitter[+C] = Splitter[JsonEvent, C]

	implicit class JsonSplitterApplyOps(val splitter: Splitter.type) extends AnyVal {
		/** Create a Splitter for JsonEvents using the given `matcher` to determine where sub-streams start and end.
		  * For example, `Splitter.json("foo")`, when applied to the json:
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
		  * @param pos     Used to construct a SpacFrameElement if a parser constructed from this splitter fails
		  * @tparam C The type of the "context" matched by the `matcher`
		  * @return A new JsonSplitter that will split a stream into sub-streams identified by the `matcher`
		  */
		def json[C](matcher: ContextMatcher[JsonStackElem, C])(implicit pos: CallerPos): JsonSplitter[C] = splitter.fromMatcher(matcher)
	}

	// ------------------------------------------------------

	type JsonContextMatcher[+C] = ContextMatcher[JsonStackElem, C]

	implicit def field(name: String): JsonContextMatcher[Unit] = new ObjectFieldContextMatcher[Unit](name, field => if (field.fieldName == name) Some(()) else None)
	def field[A](contextFromName: String => Option[A]): JsonContextMatcher[A] = new ObjectFieldContextMatcher[A]("field<>", field => contextFromName(field.fieldName))
	def fieldWhere(f: String => Boolean): JsonContextMatcher[String] = new ObjectFieldContextMatcher[String]("fieldWhere<>", field => Some(field.fieldName).filter(f))
	def anyField: JsonContextMatcher[String] = new ObjectFieldContextMatcher[String]("anyField", field => Some(field.fieldName))

	def index(i: Int): JsonContextMatcher[Unit] = new ArrayIndexContextMatcher[Unit](s"index($i)", e => if (e.index == i) Some(()) else None)
	def index[A](contextFromIndex: Int => Option[A]): JsonContextMatcher[A] = new ArrayIndexContextMatcher[A]("index<>", e => contextFromIndex(e.index))
	def indexWhere(f: Int => Boolean): JsonContextMatcher[Int] = new ArrayIndexContextMatcher[Int]("indexWhere<>", e => Some(e.index).filter(f))
	def anyIndex: JsonContextMatcher[Int] = new ArrayIndexContextMatcher[Int]("anyIndex", e => Some(e.index))
}
