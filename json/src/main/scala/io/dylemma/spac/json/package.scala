package io.dylemma.spac

import io.dylemma.spac.json.impl.{ArrayIndexContextMatcher, JsonParserTypedFirst, ObjectFieldContextMatcher}
import org.tpolecat.typename.TypeName

import scala.language.implicitConversions

/** This package provides extensions to the core "spac" library which allow for the handling of JSON data.
  *
  * Rather than creating explicit classes that extend `Parser`, `Transformer`, and `Splitter`,
  * this package provides type aliases and implicit extensions.
  * For example, `JsonParser[A]` is just a type alias for `Parser[JsonEvent, A]`,
  * and `JsonParser` is just a call to `Parser[JsonEvent]`.
  *
  * Implicit JsonParsers are available for each of the JSON primitive types:
  *
  *  - `string`
  *  - `number` (expressed as `Int`, `Long`, `Float`, or `Double`)
  *  - `boolean`
  *  - `null` (expressed as `None.type`)
  *
  * Helpers are available for parsing JSON arrays and objects:
  *
  *  - `JsonParser.listOf[A]` to parse an `array`` where each value is an `A`
  *  - `JsonParser.objectOf[A]` to parse an `object` where the value for each field an `A`
  *  - `JsonParser.objectOfNullable[A]` to parse an `object` where the value for each field is either `null` or an `A`, filtering out the `null`s
  *  - `JsonParser.fieldOf[A](fieldName)` to parse a specific field from an object
  *
  * A DSL for creating json-specific ContextMatchers is provided to make it more convenient to call `Splitter.json`.
  * For example:
  * {{{
  *     Splitter.json("foo" \ "bar").as[String].parseFirst
  * }}}
  * Can be used to capture `rootJson.foo.bar` as a String in
  * {{{
  *  {
  *    "foo": {
  *      "bar": "hello"
  *    }
  *  }
  * }}}
  *
  * To "split" values inside arrays, index-related context matchers are available, e.g.
  * {{{
  *     Splitter.json("foo" \ anyIndex).as[Int].parseToList
  * }}}
  * Can be used to capture each of the numbers in the "foo" array in
  * {{{
  *  {
  *    "foo": [1, 2, 3]
  *  }
  * }}}
  *
  * A note about JsonEvents in spac:
  * JSON doesn't have any explicit markers for when a field ends, or when an array index starts or ends;
  * those context changes are essentially inferred by the presence of some other event.
  * For example, instead of a "field end" event, typically there will be either a new "field start" or a token representing the end of the current object.
  * With spac, splitters and context matchers generally operate under the assumption that a "stack push" event (like a field start) will eventually be
  * followed by a corresponding "stack pop" event (i.e. field end).
  *
  * To allow for this, these "inferred" events (FieldEnd, IndexStart, IndexEnd) are explicitly represented as JsonEvents in the stream being parsed.
  * Keep this in mind when creating JSON ContextMatchers:
  *
  *  - `field`-related matchers will match a stack like `case ObjectStart :: FieldStart(_) :: _`
  *  - `index`-related matchers will match a stack like `case ArrayStart :: IndexStart(_) :: _`
  *
  * @groupname aliases JSON-specific Type and Value aliases
  * @groupname extensions JSON-specific extensions for Parser and Splitter
  * @groupname contextMatcherSyntax JSON Context Matcher Construction
  * @groupname event JSON Event Representation
  * @groupname support Backend Parser Support
  * @groupprio extensions 0
  * @groupprio aliases 1
  * @groupprio contextMatcherSyntax 2
  * @groupprio event 3
  * @groupprio support 4
  */
package object json {

	/** Like the `Parser` companion object, but only for creating Parsers whose input type is `JsonEvent`.
	  *
	  * @see [[JsonParserApplyOps]]
	  * @group aliases
	  */
	val JsonParser: ParserApplyWithBoundInput[JsonEvent] = Parser[JsonEvent]

	/** Type alias for a `Parser` whose input type is `JsonEvent`
	  *
	  * @group aliases
	  */
	type JsonParser[+Out] = Parser[JsonEvent, Out]

	/** JSON-specific Parser constructor methods, e.g. `JsonParser.fieldOf`
	  *
	  * @group extensions
	  */
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
			.expectInputs[JsonEvent](List("a '{' token" -> {_.isObjectStart}))
			.withName(s"JsonParser.fieldOf[${implicitly[TypeName[T]].value}]($fieldName)")

		def nullableFieldOf[T: TypeName : JsonParser](fieldName: String)(implicit callerPos: CallerPos): JsonParser[Option[T]] = nullableFieldOf[T](fieldName, implicitly[JsonParser[T]])
		def nullableFieldOf[T: TypeName](fieldName: String, parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[Option[T]] = Splitter.json(fieldName).joinBy(nullable(parser)).parseFirstOpt
			.map(_.flatten)
			.expectInputs[JsonEvent](List("a '{' token" -> {_.isObjectStart}))
			.withName(s"JsonParser.nullableFieldOf[${implicitly[TypeName[T]].value}]($fieldName)")

		def listOf[T: TypeName : JsonParser](implicit callerPos: CallerPos): JsonParser[List[T]] = listOf[T](implicitly[JsonParser[T]])
		def listOf[T: TypeName](parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[List[T]] = Splitter.json(anyIndex).joinBy(parser).parseToList
			.expectInputs[JsonEvent](List("a '[' token" -> {_.isArrayStart}))
			.withName(s"JsonParser.listOf[${implicitly[TypeName[T]].value}]")

		def objectOf[T: TypeName : JsonParser](implicit callerPos: CallerPos): JsonParser[Map[String, T]] = objectOf[T](implicitly[JsonParser[T]])
		def objectOf[T: TypeName](parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[Map[String, T]] = Splitter.json(anyField)
			.map { field => parser.map(field -> _) }
			.parseToMap
			.expectInputs[JsonEvent](List("a '{' token" -> {_.isObjectStart}))
			.withName(s"JsonParser.objectOf[${implicitly[TypeName[T]].value}]")

		def objectOfNullable[T: TypeName : JsonParser](implicit callerPos: CallerPos): JsonParser[Map[String, T]] = objectOfNullable[T](implicitly[JsonParser[T]])
		def objectOfNullable[T: TypeName](parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[Map[String, T]] = Splitter.json(anyField)
			.map { field => nullable(parser).map(field -> _) }
			.collect { case (field, Some(value)) => field -> value }
			.parseToMap
			.expectInputs[JsonEvent](List("a '{' token" -> {_.isObjectStart}))
			.withName(s"JsonParser.objectOfNullable[${implicitly[TypeName[T]].value}]")
	}

	/** @group extensions */
	implicit val jsonParserForPrimitiveString: JsonParser[String] = JsonParser.forPrimitive("a String value", _.asString.map(_.stringValue))
	/** @group extensions */
	implicit val jsonParserForPrimitiveInt: JsonParser[Int] = JsonParser.forPrimitive("an Int value", _.asLong.map(_.longValue.intValue))
	/** @group extensions */
	implicit val jsonParserForPrimitiveLong: JsonParser[Long] = JsonParser.forPrimitive("a Long value", _.asLong.map(_.longValue))
	/** @group extensions */
	implicit val jsonParserForPrimitiveFloat: JsonParser[Float] = JsonParser.forPrimitive("a Float value", _.asDouble.map(_.doubleValue.floatValue))
	/** @group extensions */
	implicit val jsonParserForPrimitiveDouble: JsonParser[Double] = JsonParser.forPrimitive("a Double value", _.asDouble.map(_.doubleValue))
	/** @group extensions */
	implicit val jsonParserForPrimitiveBoolean: JsonParser[Boolean] = JsonParser.forPrimitive("a Boolean value", _.asBool.map(_.booleanValue))
	/** @group extensions */
	implicit val jsonParserForPrimitiveNull: JsonParser[None.type] = JsonParser.forPrimitive("a Null value", _.asNull.map(_ => None))

	/** @group aliases */
	val JsonTransformer: TransformerApplyWithBoundInput[JsonEvent] = Transformer[JsonEvent]
	/** @group aliases */
	type JsonTransformer[+Out] = Transformer[JsonEvent, Out]

	/** @group aliases */
	val JsonSplitter: SplitterApplyWithBoundInput[JsonEvent] = Splitter[JsonEvent]
	/** @group aliases */
	type JsonSplitter[+C] = Splitter[JsonEvent, C]

	/** Adds `Splitter.json`, for constructing json context matcher-based JsonSplitters
	  *
	  * @group extensions
	  */
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

	/** @group aliases */
	type JsonContextMatcher[+C] = ContextMatcher[JsonStackElem, C]

	/** @group contextMatcherSyntax */
	implicit def field(name: String): JsonContextMatcher[Unit] = new ObjectFieldContextMatcher[Unit](name, field => if (field.fieldName == name) Some(()) else None)
	/** @group contextMatcherSyntax */
	def field[A](contextFromName: String => Option[A]): JsonContextMatcher[A] = new ObjectFieldContextMatcher[A]("field<>", field => contextFromName(field.fieldName))
	/** @group contextMatcherSyntax */
	def fieldWhere(f: String => Boolean): JsonContextMatcher[String] = new ObjectFieldContextMatcher[String]("fieldWhere<>", field => Some(field.fieldName).filter(f))
	/** @group contextMatcherSyntax */
	def anyField: JsonContextMatcher[String] = new ObjectFieldContextMatcher[String]("anyField", field => Some(field.fieldName))

	/** @group contextMatcherSyntax */
	def index(i: Int): JsonContextMatcher[Unit] = new ArrayIndexContextMatcher[Unit](s"index($i)", e => if (e.index == i) Some(()) else None)
	/** @group contextMatcherSyntax */
	def index[A](contextFromIndex: Int => Option[A]): JsonContextMatcher[A] = new ArrayIndexContextMatcher[A]("index<>", e => contextFromIndex(e.index))
	/** @group contextMatcherSyntax */
	def indexWhere(f: Int => Boolean): JsonContextMatcher[Int] = new ArrayIndexContextMatcher[Int]("indexWhere<>", e => Some(e.index).filter(f))
	/** @group contextMatcherSyntax */
	def anyIndex: JsonContextMatcher[Int] = new ArrayIndexContextMatcher[Int]("anyIndex", e => Some(e.index))
}
