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

	/** Provides JSON-specific Parser constructor methods to the `JsonParser` object, for example `JsonParser.fieldOf`.
	  *
	  * Technically `JsonParser` is not a companion object, it is a partially-applied version of the `Parser` companion
	  * object which binds the input type to `JsonEvent`, so "companion methods" must instead be added as extension methods.
	  *
	  * @group extensions
	  * @groupname primitive Primitives
	  * @groupname high Objects and Arrays
	  */
	implicit class JsonParserApplyOps(private val parserApply: ParserApplyWithBoundInput[JsonEvent]) extends AnyVal {

		/** Creates a JsonParser which captures the first JsonEvent it sees, expecting a primitive that can satisfy the `matchPrimitive` function.
		  *
		  * This is the low-level method used to implement the `forString`, `forInt`, etc parsers.
		  * Its main use outside of those is if you want to create a parser that handles multiple different primitives without resorting to the use of `orElse`.
		  *
		  * @param describePrimitive A message describing the type of event that this parser expects. Used to construct a SpacException if the `matchPrimitive` returns `None`
		  * @param matchPrimitive    A function used to extract a value from the first JsonEvent this parser's handler encounters.
		  * @tparam A The type of the extracted value
		  * @return A JsonParser which attempts to extract some value from the first event it encounters, throwing a SpacException if it cannot
		  * @group primitive
		  */
		def forPrimitive[A](describePrimitive: String, matchPrimitive: JsonEvent => Option[A]): JsonParser[A] = new JsonParserTypedFirst(describePrimitive, matchPrimitive)

		/** Wraps an existing JsonParser, creating a new JsonParser that will succeed with `None` if it encounters a `null`,
		  * or succeed with a `Some(t)` if the wrapped parser succeeds.
		  *
		  * Used to parse values that may or may not be null.
		  *
		  * Typical usage would be `JsonParser.nullable[String]`, passing the underlying `JsonParser[String]` implicitly.
		  *
		  * @param parser The underlying parser which would typically fail upon encountering a `null`
		  * @tparam T The type of the underlying parser's extracted value
		  * @return A JsonParser which parses `null` as `None`, or else delegates to the underlying `parser`
		  * @group primitive
		  */
		def nullable[T](implicit parser: JsonParser[T]): JsonParser[Option[T]] = parserApply.oneOf(parser.map(Some(_)), forNull)

		/** A JsonParser that captures the string value from a `JsString` event, failing if the first event is not a `JsString`.
		  *
		  * @group primitive
		  */
		def forString: JsonParser[String] = jsonParserForPrimitiveString

		/** A JsonParser that captures a `JLong` event as an `Int` value, failing if the first event is not a `JLong`.
		  *
		  * @group primitive
		  */
		def forInt: JsonParser[Int] = jsonParserForPrimitiveInt

		/** A JsonParser that captures a `JLong` event as a `Long` value, failing if the first event is not a `JLong`.
		  *
		  * @group primitive
		  */
		def forLong: JsonParser[Long] = jsonParserForPrimitiveLong

		/** A JsonParser that captures a `JDouble` event as a `Float` value, failing if the first event is not a `JDouble`.
		  *
		  * @group primitive
		  */
		def forFloat: JsonParser[Float] = jsonParserForPrimitiveFloat

		/** A JsonParser that captures a `JDouble` event as a `Double` value, failing if the first event is not a `JDouble`.
		  *
		  * @group primitive
		  */
		def forDouble: JsonParser[Double] = jsonParserForPrimitiveDouble

		/** A JsonParser that captures the `Boolean` value from a `JBool` event, failing if the first event is not a `JBool`.
		  *
		  * @group primitive
		  */
		def forBoolean: JsonParser[Boolean] = jsonParserForPrimitiveBoolean

		/** A JsonParser that returns `None` upon encountering a `JNull` event, failing upon encountering any other event or an EOF.
		  *
		  * @group primitive
		  */
		def forNull: JsonParser[None.type] = jsonParserForPrimitiveNull

		/** A JsonParser that parses a JSON object by parsing the given mandatory field with the implicitly-available `JsonParser[T]`.
		  *
		  * The returned parser will fail if the first event is not an `ObjectStart`,
		  * or if the expected field does not appear inside the object,
		  * or if the value of the expected field causes the underlying parser to fail.
		  *
		  * This is a shortcut for `Splitter.json(fieldName).as[T].parseFirst`.
		  *
		  * E.g.
		  * {{{
		  *    val rawJson = """{ "foo": 1, "bar": true }"""
		  *    val parser = JsonParser.fieldOf[Boolean]("bar")
		  *    parser.parse(rawJson) // returns `true`
		  * }}}
		  *
		  * @param fieldName The name of the expected field
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the extracted value
		  * @return A JsonParser that parses the given field from a JSON object as a value of type `T`
		  * @group high
		  */
		def fieldOf[T: TypeName : JsonParser](fieldName: String)(implicit callerPos: CallerPos): JsonParser[T] = fieldOf[T](fieldName, implicitly[JsonParser[T]])

		/** A JsonParser that parses a JSON object by parsing the given mandatory field with the given `JsonParser[T]`.
		  *
		  * Note that if the given `parser` is available implicitly, you can use the other `fieldOf` signature.
		  *
		  * The returned parser will fail if the first event is not an `ObjectStart`,
		  * or if the expected field does not appear inside the object,
		  * or if the value of the expected field causes the underlying parser to fail.
		  *
		  * This is a shortcut for `Splitter.json(fieldName).as[T].parseFirst`.
		  *
		  * E.g.
		  * {{{
		  *    val rawJson = """{ "foo": 1, "bar": true }"""
		  *    val parser = JsonParser.fieldOf[Boolean]("bar")
		  *    parser.parse(rawJson) // returns `true`
		  * }}}
		  *
		  * @param fieldName The name of the expected field
		  * @param parser    The underlying parser used to parse the value inside the expected field in the object
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the extracted value
		  * @return A JsonParser that parses the given field from a JSON object as a value of type `T`
		  * @group high
		  */
		def fieldOf[T: TypeName](fieldName: String, parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[T] = Splitter.json(fieldName).joinBy(parser).parseFirst
			.expectInputs[JsonEvent](List("a '{' token" -> {_.isObjectStart}))
			.withName(s"JsonParser.fieldOf[${implicitly[TypeName[T]].value}]($fieldName)")

		/** A JsonParser that parses a JSON object by parsing the given optional field with an implicitly-available `JsonParser[T]`.
		  *
		  * Unlike with `fieldOf`, the returned parser will succeed with a `None` if the expected field is missing,
		  * or if the value in the expected field is null.
		  * However, it will still fail if the first event is not an `ObjectStart`.
		  *
		  * E.g.
		  * {{{
		  *    val parser = JsonParser.nullableFieldOf[Int]("foo")
		  *    parser.parse("{}") // returns None
		  *    parser.parse(12) // throws a SpacException
		  *    parser.parse("""{ "foo": 42 }""") // returns 42
		  *    parser.parse("""{ "foo": null }""") // returns null
		  *    parser.parse("""{ "foo": "hello" }""") // throws a SpacException
		  * }}}
		  *
		  * @param fieldName The name of the field
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the extracted value
		  * @return A JsonParser that parses the given field from a JSON object, wrapping a successfully-parsed value in `Some`, and treating null or a missing field as `None`
		  * @group high
		  */
		def nullableFieldOf[T: TypeName : JsonParser](fieldName: String)(implicit callerPos: CallerPos): JsonParser[Option[T]] = nullableFieldOf[T](fieldName, implicitly[JsonParser[T]])

		/** A JsonParser that parses a JSON object by parsing the given optional field with the given `parser`.
		  *
		  * Note that if the given `parser` is available implicitly, you can use the other `nullableFieldOf` signature.
		  *
		  * Unlike with `fieldOf`, the returned parser will succeed with a `None` if the expected field is missing,
		  * or if the value in the expected field is null.
		  * However, it will still fail if the first event is not an `ObjectStart`.
		  *
		  * E.g.
		  * {{{
		  *    val parser = JsonParser.nullableFieldOf("foo", JsonParser.forInt)
		  *    parser.parse("{}") // returns None
		  *    parser.parse("12") // throws a SpacException
		  *    parser.parse("""{ "foo": 42 }""") // returns 42
		  *    parser.parse("""{ "foo": null }""") // returns null
		  *    parser.parse("""{ "foo": "hello" }""") // throws a SpacException
		  * }}}
		  *
		  * @param fieldName The name of the field
		  * @param parser    The underlying parser used to parse the value inside the expected field in the object
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the extracted value
		  * @return A JsonParser that parses the given field from a JSON object, wrapping a successfully-parsed value in `Some`, and treating null or a missing field as `None`
		  * @group high
		  */
		def nullableFieldOf[T: TypeName](fieldName: String, parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[Option[T]] = Splitter.json(fieldName).joinBy(nullable(parser)).parseFirstOpt
			.map(_.flatten)
			.expectInputs[JsonEvent](List("a '{' token" -> {_.isObjectStart}))
			.withName(s"JsonParser.nullableFieldOf[${implicitly[TypeName[T]].value}]($fieldName)")

		/** A JsonParser that parses a JSON array by parsing each item in the array via the implicitly-available `JsonParser[T]`,
		  * collecting the values to a List.
		  *
		  * This is a shortcut for `Splitter.json(anyIndex).as[T].parseToList`.
		  *
		  * The returned parser will fail if the first event is not an `ArrayStart`,
		  * or if any of the values inside the array cause the underlying parser to fail.
		  * E.g. if the underlying parser is `JsonParser.forInt`, but one of the values in the array is a string,
		  * the exception thrown by `JsonParser.forInt` will bubble up through the returned parser.
		  *
		  * E.g.
		  * {{{
		  *    val parser = JsonParser.listOf[Int]
		  *    parser.parse("[1, 2, 3]") // returns List(1, 2, 3)
		  *    parser.parse("[]") // returns Nil
		  *    parser.parse("42") // throws a SpacException
		  *    parser.parse("[1, 2, false]") // throws a SpacException
		  * }}}
		  *
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the values inside the array
		  * @return A JsonParser that parses an array of values as a List[T]
		  * @group high
		  */
		def listOf[T: TypeName : JsonParser](implicit callerPos: CallerPos): JsonParser[List[T]] = listOf[T](implicitly[JsonParser[T]])

		/** A JsonParser that parses a JSON array by parsing each item in the array via the given `parser`,
		  * collecting the values to a List.
		  *
		  * Note that if the given `parser` is available implicitly, you can use the other `listOf` signature instead.
		  *
		  * This is a shortcut for `Splitter.json(anyIndex).joinBy(parser).parseToList`.
		  *
		  * The returned parser will fail if the first event is not an `ArrayStart`,
		  * or if any of the values inside the array cause the underlying parser to fail.
		  * E.g. if the underlying parser is `JsonParser.forInt`, but one of the values in the array is a string,
		  * the exception thrown by `JsonParser.forInt` will bubble up through the returned parser.
		  *
		  * E.g.
		  * {{{
		  *    val parser = JsonParser.listOf(JsonParser.forInt)
		  *    parser.parse("[1, 2, 3]") // returns List(1, 2, 3)
		  *    parser.parse("[]") // returns Nil
		  *    parser.parse("42") // throws a SpacException
		  *    parser.parse("[1, 2, false]") // throws a SpacException
		  * }}}
		  *
		  * @param parser    The underlying parser to use for each value inside the array
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the values inside the array
		  * @return A JsonParser that parses an array of values as a List[T]
		  * @group high
		  */
		def listOf[T: TypeName](parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[List[T]] = Splitter.json(anyIndex).joinBy(parser).parseToList
			.expectInputs[JsonEvent](List("a '[' token" -> {_.isArrayStart}))
			.withName(s"JsonParser.listOf[${implicitly[TypeName[T]].value}]")

		/** A JsonParser that parses a JSON object by interpreting every field as a value of type `T` using the implicitly-available `JsonParser[T]`,
		  * yielding a `Map` containing the parsed `field -> value` pairs.
		  *
		  * This is a shortcut for `Splitter.json(anyField).map(field -> implicitly[JsonParser[T]].map(field -> _)).parseToMap`.
		  *
		  * The returned parser will fail if the first event is not an `ObjectStart`,
		  * or if any of the field values in the object cause the underlying parser to fail.
		  * E.g. if the underlying parser is `JsonParser.forString`, but one of the fields contains some non-string value,
		  * the exception thrown by `JsonParser.forString` will bubble up through the returned parser.
		  *
		  * E.g.
		  * {{{
		  *    val parser = JsonParser.objectOf[Int]
		  *    parser.parse("""{ "foo": 1, "bar": 2 }""") // returns Map("foo" -> 1, "bar" -> 2)
		  *    parser.parse("""{ "foo": 1, "bar": "whoops" }""") // throws a SpacException
		  *    parser.parse("13") // throws a SpacException
		  * }}}
		  *
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the values inside each field
		  * @return A JsonParser that parses an object as a `Map[String, T]`
		  * @group high
		  */
		def objectOf[T: TypeName : JsonParser](implicit callerPos: CallerPos): JsonParser[Map[String, T]] = objectOf[T](implicitly[JsonParser[T]])

		/** A JsonParser that parses a JSON object by interpreting every field as a value of type `T` using the given `parser`,
		  * yielding a `Map` containing the parsed `field -> value` pairs.
		  *
		  * Note that if the given `parser` is available implicitly, you can use the other `objectOf` signature instead.
		  *
		  * This is a shortcut for `Splitter.json(anyField).map(field -> parser.map(field -> _)).parseToMap`.
		  *
		  * The returned parser will fail if the first event is not an `ObjectStart`,
		  * or if any of the field values in the object cause the underlying parser to fail.
		  * E.g. if the underlying parser is `JsonParser.forString`, but one of the fields contains some non-string value,
		  * the exception thrown by `JsonParser.forString` will bubble up through the returned parser.
		  *
		  * E.g.
		  * {{{
		  *    val parser = JsonParser.objectOf(JsonParser.forInt)
		  *    parser.parse("""{ "foo": 1, "bar": 2 }""") // returns Map("foo" -> 1, "bar" -> 2)
		  *    parser.parse("""{ "foo": 1, "bar": "whoops" }""") // throws a SpacException
		  *    parser.parse("13") // throws a SpacException
		  * }}}
		  *
		  * @param parser    The underlying parser used to parse each field in the object
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the values inside each field
		  * @return A JsonParser that parses an object as a `Map[String, T]`
		  * @group high
		  */
		def objectOf[T: TypeName](parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[Map[String, T]] = Splitter.json(anyField)
			.map { field => parser.map(field -> _) }
			.parseToMap
			.expectInputs[JsonEvent](List("a '{' token" -> {_.isObjectStart}))
			.withName(s"JsonParser.objectOf[${implicitly[TypeName[T]].value}]")

		/** A JsonParser that parses a JSON object by interpreting every possibly-null field as a value of type `T` using
		  * an implicitly-available `JsonParser[T]`, filtering out null fields and yielding a `Map` containing `field -> value` pairs
		  * of the successfully-parsed fields.
		  *
		  * Note that while fields with `null` instead of an expected `T` value are filtered out,
		  * non-null fields that cause the underlying parser to fail will cause the returned parser to fail as well.
		  * As with `objectOf`, the returned parser will fail if the first event is not an `ObjectStart`.
		  *
		  * E.g.
		  * {{{
		  *    val parser = JsonParser.objectOfNullable[Int]
		  *    parser.parse("""{ "foo": 1, "bar": 2 }""") // returns Map("foo" -> 1, "bar" -> 2)
		  *    parser.parse("""{ "foo": 1, "bar": null }""") // returns Map("foo" -> 1)
		  *    parser.parse("""{ "foo": 1, "bar": "whoops" }""") // throws a SpacException
		  *    parser.parse("13") // throws a SpacException
		  * }}}
		  *
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the values inside each field
		  * @return A JsonParser that parses an object as a `Map[String, T]`, ignoring fields with `null` values
		  * @group high
		  */
		def objectOfNullable[T: TypeName : JsonParser](implicit callerPos: CallerPos): JsonParser[Map[String, T]] = objectOfNullable[T](implicitly[JsonParser[T]])

		/** A JsonParser that parses a JSON object by interpreting every possibly-null field as a value of type `T` using
		  * the given `JsonParser[T]`, filtering out null fields and yielding a `Map` containing `field -> value` pairs
		  * of the successfully-parsed fields.
		  *
		  * Note that if the given `parser` is available implicitly, you can use the other `objectOfNullable` signature instead.
		  *
		  * Note that while fields with `null` instead of an expected `T` value are filtered out,
		  * non-null fields that cause the underlying parser to fail will cause the returned parser to fail as well.
		  * As with `objectOf`, the returned parser will fail if the first event is not an `ObjectStart`.
		  *
		  * E.g.
		  * {{{
		  *    val parser = JsonParser.objectOfNullable(JsonParser.forInt)
		  *    parser.parse("""{ "foo": 1, "bar": 2 }""") // returns Map("foo" -> 1, "bar" -> 2)
		  *    parser.parse("""{ "foo": 1, "bar": null }""") // returns Map("foo" -> 1)
		  *    parser.parse("""{ "foo": 1, "bar": "whoops" }""") // throws a SpacException
		  *    parser.parse("13") // throws a SpacException
		  * }}}
		  *
		  * @param parser    The underlying parser used to parse values (aside from null) for each of the fields in the input object
		  * @param callerPos Macro-derived location of the code calling this method, used to form a SpacTraceElement when the returned parser fails
		  * @tparam T The type of the values inside each field
		  * @return A JsonParser that parses an object as a `Map[String, T]`, ignoring fields with `null` values
		  * @group high
		  */
		def objectOfNullable[T: TypeName](parser: JsonParser[T])(implicit callerPos: CallerPos): JsonParser[Map[String, T]] = Splitter.json(anyField)
			.map { field => nullable(parser).map(field -> _) }
			.collect { case (field, Some(value)) => field -> value }
			.parseToMap
			.expectInputs[JsonEvent](List("a '{' token" -> {_.isObjectStart}))
			.withName(s"JsonParser.objectOfNullable[${implicitly[TypeName[T]].value}]")
	}

	/** Implicit version of `JsonParser.forString`
	  *
	  * @group extensions
	  */
	implicit val jsonParserForPrimitiveString: JsonParser[String] = JsonParser.forPrimitive("a String value", _.asString.map(_.stringValue))

	/** Implicit version of `JsonParser.forInt`
	  *
	  * @group extensions
	  */
	implicit val jsonParserForPrimitiveInt: JsonParser[Int] = JsonParser.forPrimitive("an Int value", _.asLong.map(_.longValue.intValue))

	/** Implicit version of `JsonParser.forLong`
	  *
	  * @group extensions
	  */
	implicit val jsonParserForPrimitiveLong: JsonParser[Long] = JsonParser.forPrimitive("a Long value", _.asLong.map(_.longValue))

	/** Implicit version of `JsonParser.forFloat`
	  *
	  * @group extensions
	  */
	implicit val jsonParserForPrimitiveFloat: JsonParser[Float] = JsonParser.forPrimitive("a Float value", _.asDouble.map(_.doubleValue.floatValue))

	/** Implicit version of `JsonParser.forDouble`
	  *
	  * @group extensions
	  */
	implicit val jsonParserForPrimitiveDouble: JsonParser[Double] = JsonParser.forPrimitive("a Double value", _.asDouble.map(_.doubleValue))

	/** Implicit version of `JsonParser.forBoolean`
	  *
	  * @group extensions
	  */
	implicit val jsonParserForPrimitiveBoolean: JsonParser[Boolean] = JsonParser.forPrimitive("a Boolean value", _.asBool.map(_.booleanValue))

	/** Implicit version of `JsonParser.forNull`
	  *
	  * @group extensions
	  */
	implicit val jsonParserForPrimitiveNull: JsonParser[None.type] = JsonParser.forPrimitive("a Null value", _.asNull.map(_ => None))

	/** Like the `Transformer` companion object, but only for creating `Transformers` whose input type is `JsonEvent`.
	  *
	  * @group aliases
	  */
	val JsonTransformer: TransformerApplyWithBoundInput[JsonEvent] = Transformer[JsonEvent]

	/** Type alias for a `Transformer` whose input type is `JsonEvent`.
	  *
	  * @group aliases
	  */
	type JsonTransformer[+Out] = Transformer[JsonEvent, Out]

	/** Like the `Splitter` companion object, but only for creating `Splitters` whose input type is `JsonEvent`.
	  *
	  * @group aliases
	  */
	val JsonSplitter: SplitterApplyWithBoundInput[JsonEvent] = Splitter[JsonEvent]

	/** Type alias for a `Splitter` whose input type is `JsonEvent`
	  *
	  * @group aliases
	  */
	type JsonSplitter[+C] = Splitter[JsonEvent, C]

	/** Adds `Splitter.json`, for constructing json context matcher-based JsonSplitters
	  *
	  * @group extensions
	  */
	implicit class JsonSplitterApplyOps(private val splitter: Splitter.type) extends AnyVal {
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

	/** Adds `splitter.asNullable[A]`, for handling possibly-null values in a JSON substream
	  *
	  * @group extensions
	  */
	implicit class JsonSplitterOps[C](private val splitter: JsonSplitter[C]) extends AnyVal {

		/** Alternative to `splitter.as[A]` which also accepts `null` in place of the normal JSON for `A`.
		  *
		  * @tparam A Any type for which an implicit `JsonParser[A]` exists
		  */
		def asNullable[A: JsonParser]: JsonTransformer[Option[A]] = splitter.joinBy(JsonParser.nullable[A])
	}

	// ------------------------------------------------------

	/** @group aliases */
	type JsonContextMatcher[+C] = ContextMatcher[JsonStackElem, C]

	/** Context matcher that matches events within a given JSON object field.
	  * Specifically this matcher looks for an `ObjectStart` followed by a `FieldStart(name)` in the JSON context stack.
	  *
	  * When calling `Splitter.json`, the fact that this method is `implicit` allows you to use the underlying field name
	  * directly instead of directly calling this method, e.g. `Splitter.json("address" \ "street")`
	  *
	  * @param name the name of the field
	  * @group contextMatcherSyntax
	  */
	implicit def field(name: String): JsonContextMatcher[Unit] = new ObjectFieldContextMatcher[Unit](name, field => if (field.fieldName == name) Some(()) else None)

	/** Context matcher that matches events within certain JSON object fields, extracting some context value based on the name of the matched field.
	  * Specifically this matcher looks for an `ObjectStart` followed by a `FieldStart(name)`, then plugs the `name` into `contextFromName`.
	  * If `contextFromName` returns a `Some`, the match succeeds and the value inside the Some is returned.
	  *
	  * @param contextFromName a function that extracts a context value based on the name of the JSON object field
	  * @group contextMatcherSyntax
	  */
	def field[A](contextFromName: String => Option[A]): JsonContextMatcher[A] = new ObjectFieldContextMatcher[A]("field<>", field => contextFromName(field.fieldName))

	/** Context matcher that matches events within JSON object fields whose names case the given `f` predicate to return `true`.
	  * Specifically this matcher looks for an `ObjectStart` followed by a `FieldStart(name)` where `f(name)` is `true`.
	  * If the match succeeds, the name of the field is also extracted as a context value.
	  *
	  * @param f A predicate applied to field names to determine whether the current JSON context stack matches
	  * @group contextMatcherSyntax
	  */
	def fieldWhere(f: String => Boolean): JsonContextMatcher[String] = new ObjectFieldContextMatcher[String]("fieldWhere<>", field => Some(field.fieldName).filter(f))

	/** Context matcher that matches events with any JSON object field, capturing the name of the field as context.
	  * Specifically this matcher looks for an `ObjectStart` followed by a `FieldStart(name)`, for any `name`.
	  *
	  * @group contextMatcherSyntax
	  */
	def anyField: JsonContextMatcher[String] = new ObjectFieldContextMatcher[String]("anyField", field => Some(field.fieldName))

	/** Context matcher that matches events within a JSON array at a specific index.
	  * Specifically this matcher looks for an `ArrayStart` followed by an `IndexStart(i)` in the JSON context stack.
	  *
	  * @group contextMatcherSyntax
	  */
	def index(i: Int): JsonContextMatcher[Unit] = new ArrayIndexContextMatcher[Unit](s"index($i)", e => if (e.index == i) Some(()) else None)

	/** Context matcher that matches events within a JSON array, at certain indexes.
	  * Specifically this matcher looks for an `ArrayStart` followed by an `IndexStart(i)` in the JSON context stack,
	  * where the `i` from the `IndexStart` is passed to `contextFromIndex`.
	  * If `contextFromIndex(i)` returns a `Some`, the match succeeds and the value inside the `Some` is returned as a context value.
	  *
	  * @group contextMatcherSyntax
	  */
	def index[A](contextFromIndex: Int => Option[A]): JsonContextMatcher[A] = new ArrayIndexContextMatcher[A]("index<>", e => contextFromIndex(e.index))

	/** Context matcher that matches events within a JSON array, at certain indexes.
	  * Specifically this matcher looks for an `ArrayStart` followed by an `IndexStart(i)` in the JSON context stack, where `f(i)` is `true`.
	  * If the match succeeds, the `i` index will be captured as a context value.
	  *
	  * @group contextMatcherSyntax
	  */
	def indexWhere(f: Int => Boolean): JsonContextMatcher[Int] = new ArrayIndexContextMatcher[Int]("indexWhere<>", e => Some(e.index).filter(f))

	/** Context matcher that matches events within a JSON array, at any index.
	  * Specifically this matcher looks for an `ArrayStart` followed by an `IndexStart(_)` in the JSON context stack.
	  * If the match succeeds, the `i` index will be captured as a context value.
	  *
	  * @group contextMatcherSyntax
	  */
	def anyIndex: JsonContextMatcher[Int] = new ArrayIndexContextMatcher[Int]("anyIndex", e => Some(e.index))
}
