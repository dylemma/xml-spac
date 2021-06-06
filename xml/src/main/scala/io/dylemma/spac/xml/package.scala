package io.dylemma.spac

import cats.syntax.show._
import io.dylemma.spac.xml.impl._

import scala.language.implicitConversions

/** This package provides extensions to the core "spac" library which allow for the handling of XML data.
  *
  * Rather than creating explicit classes that extend `Parser`, `Transformer`, and `Splitter`,
  * this package provides type aliases and implicit extensions.
  * For example, `XmlParser[A]` is just a type alias for `Parser[XmlEvent, A]`,
  * and `XmlParser` is just a call to `Parser[XmlEvent]`.
  *
  * Three main Parser methods are added to `Parser[XmlEvent]` via the `XmlParserApplyOps` implicit class:
  *
  *  - `XmlParser.forText` - for capturing raw text
  *  - `XmlParser.attr` - for capturing mandatory attributes from elements
  *  - `XmlParser.attrOpt` - for capturing optional attributes from elements
  *
  * One main Splitter constructor method is added to `Splitter` via the `XmlSplitterApplyOps` implicit class:
  *
  *  - `Splitter.xml` - for creating splitters based on an inspection of an "element stack"
  *
  * Three main Splitter member methods are added to `Splitter[XmlEvent, C]` via the `XmlSplitterOps` implicit class:
  *
  *  - `.attr` - alias for `.joinBy(XmlParser.attr(...))`
  *  - `.attrOpt` - alias for `.joinBy(XmlParser.attrOpt(...))`
  *  - `.text` - alias for `.joinBy(XmlParser.forText)`
  *
  * A DSL for creating xml-specific ContextMatchers is provided to make it more convenient to call `Splitter.xml`.
  * For example:
  * {{{
  *  Splitter.xml("things" \ "thing").attr("foo").parseToList
  * }}}
  * Can be used to capture a list of the "foo" attributes in the `<thing>` elements in
  * {{{
  *  <things>
  *     <thing foo="hello" />
  *     <thing foo="Goodbye">
  *        <extra>junk</extra>
  *     </thing>
  *  </thing>
  * }}}
  *
  * @groupname aliases XML-specific Type and Value aliases
  * @groupname extensions XML-specific extensions for Parser and Splitter
  * @groupname contextMatcherSyntax XML Context Matcher Construction
  * @groupname event XML Event Representation
  * @groupname support Backend Parser Support
  * @groupprio extensions 0
  * @groupprio aliases 1
  * @groupprio contextMatcherSyntax 2
  * @groupprio event 3
  * @groupprio support 4
  */
package object xml {

	// ----------------------------------------------------------------------------
	// XmlParser - companion ops
	// ----------------------------------------------------------------------------

	/** Like the `Parser` companion object, but only for creating Parsers whose input type is `XmlEvent`.
	  *
	  * @see [[XmlParserApplyOps]]
	  * @group aliases
	  */
	val XmlParser: ParserApplyWithBoundInput[XmlEvent] = Parser[XmlEvent]
	/** Type alias for a `Parser` whose input type is `XmlEvent`.
	  *
	  * @group aliases
	  */
	type XmlParser[+Out] = Parser[XmlEvent, Out]

	@deprecated("Use `XmlParser` (with lowercase 'ml') to reference the parser companion", "v0.9")
	val XMLParser = XmlParser
	@deprecated("Use `XmlParser` (with lowercase 'ml') instead", "v0.9")
	type XMLParser[+Out] = XmlParser[Out]

	/** XML-specific Parser constructor methods, for example `XmlParser.attr` and `XmlParser.text`
	  *
	  * @group extensions
	  */
	implicit class XmlParserApplyOps(val parserApply: ParserApplyWithBoundInput[XmlEvent]) extends AnyVal {
		/** An XmlParser that concatenates all of the raw character data found in the XmlEvent stream it consumes.
		  */
		def forText: Parser[XmlEvent, String] = XmlParserText

		/** An XmlParser that extracts the requested mandatory attribute from the first `ElemStart` in the stream it consumes,
		  * throwing an exception if the attribute is missing or an `ElemStart` never appears.
		  *
		  * @param attributeName The name of the requested attribute
		  * @tparam N Usually a `String` but can be a `QName` from whichever parser backend you're using
		  * @return A parser that extracts the given mandatory attribute from an XML element
		  */
		def forMandatoryAttribute[N: AsQName](attributeName: N): Parser[XmlEvent, String] = new XmlParserMandatoryAttribute(attributeName)

		/** An XmlParser that extracts the requested optional attribute from the first `ElemStart` in the stream it consumes, wrapped in a `Some`,
		  * instead yielding a `None` if the attribute is missing or an `ElemStart` never appears.
		  *
		  * @param attributeName The name of the requested attribute
		  * @tparam N Usually a `String` but can be a `QName` from whichever parser backend you're using
		  * @return A parser that extracts the given optional attribute from an XML element
		  */
		def forOptionalAttribute[N: AsQName](attributeName: N): Parser[XmlEvent, Option[String]] = new XmlParserOptionalAttribute(attributeName)

		/** Alias for `forMandatoryAttribute`.
		  *
		  * @param attributeName The name of the requested attribute
		  * @tparam N Usually a `String` but can be a `QName` from whichever parser backend you're using
		  * @return A parser that extracts the given mandatory attribute from an XML element
		  */
		def attr[N: AsQName](attributeName: N): XmlParser[String] = forMandatoryAttribute(attributeName)

		/** Alias for `forOptionalAttribute`.
		  *
		  * @param attributeName The name of the requested attribute
		  * @tparam N Usually a `String` but can be a `QName` from whichever parser backend you're using
		  * @return A parser that extracts the given optional attribute from an XML element
		  */
		def attrOpt[N: AsQName](attributeName: N): XmlParser[Option[String]] = forOptionalAttribute(attributeName)
	}

	/** @group aliases */
	val XmlTransformer: TransformerApplyWithBoundInput[XmlEvent] = Transformer[XmlEvent]
	/** @group aliases */
	type XmlTransformer[+Out] = Transformer[XmlEvent, Out]

	// ----------------------------------------------------------------------------
	// Splitter & XmlSplitter - companion ops
	// ----------------------------------------------------------------------------

	/** @group aliases */
	val XmlSplitter: SplitterApplyWithBoundInput[XmlEvent] = Splitter[XmlEvent]
	/** @group aliases */
	type XmlSplitter[+C] = Splitter[XmlEvent, C]

	@deprecated("Use `XmlSplitter` (with lowercase 'ml') for directly referencing the companion object, or use `Splitter.xml` to construct a new XmlSplitter", "v0.9")
	val XMLSplitter = XmlSplitter
	@deprecated("Use `XmlSplitter` (with lowercase 'ml') instead", "v0.9")
	type XMLSplitter[+C] = XmlSplitter[C]

	/** Adds `Splitter.xml`, for constructing element matcher-based XmlSplitters.
	  *
	  * @group extensions
	  */
	implicit class XmlSplitterApplyOps(val splitter: Splitter.type) extends AnyVal {

		/** Creates a Splitter over `XmlEvent`s using the given `matcher` to determine where sub-streams start and end.
		  * For example, `Splitter.xml(* \ "foo")` when applied to the xml:
		  * {{{
		  * <elem>
		  *    <foo>hello</foo>
		  *    <foo>goodbye</foo>
		  * </elem>
		  * }}}
		  * would identify the first and second `<foo>` elements as separate substreams, containing
		  * the events `ElemStart(foo), Text(hello), ElemEnd(foo)` and `ElemStart(foo), Text(goodbye), ElemEnd(foo)` respectively.
		  *
		  * Any context matched by the `matcher` will be passed to the "joiner" functions, e.g. `flatMap` and `map`.
		  *
		  * @param matcher A ContextMatcher used to identify where each sub-stream begins and ends,
		  *                and extracts some context value to identify each sub-stream.
		  * @param pos     Used to construct a SpacFrameElement if a parser constructed from this splitter fails
		  * @tparam C The context type returned by the `matcher`
		  * @return A new Splitter that splits the source into sub-streams identified by the `matcher`
		  */
		def xml[C](matcher: ContextMatcher[XmlEvent.ElemStart, C])(implicit pos: CallerPos): XmlSplitter[C] = splitter.fromMatcher(matcher)
	}

	// ----------------------------------------------------------------------------
	// XmlSplitter - member ops
	// ----------------------------------------------------------------------------

	/** XML-specific Splitter member methods, for example: `attr`, `attrOpt`, and `text`,
	  *
	  * @group extensions
	  */
	implicit class XmlSplitterOps[C](splitter: Splitter[XmlEvent, C]) {
		/** Consumes each sub-stream with the `XmlParser.forMandatoryAttribute(name)` parser,
		  * yielding an XmlTransformer[String] which emits the resulting attribute from those sub-streams.
		  *
		  * @param name The name of the attribute to capture
		  * @tparam N The name *type* of the attribute to capture. This will generally be `String` but may
		  *           be any other type `N` that belongs to the `AsQName` typeclass.
		  * @return An XmlTransformer[String] that emits the given mandatory attribute from the first element in each matched sub-stream
		  */
		def attr[N: AsQName](name: N): Transformer[XmlEvent, String] = splitter.joinBy(XmlParser.forMandatoryAttribute(name))

		/** Consumes each sub-stream with the `XmlParser.forOptionalAttribute(name)` parser,
		  * yielding an XmlTransformer[String] which emits the resulting attribute from those sub-streams.
		  *
		  * @param name The name of the attribute to capture
		  * @tparam N The name *type* of the attribute to capture. This will generally be `String` but may
		  *           be any other type `N` that belongs to the `AsQName` typeclass.
		  * @return An XmlTransformer[String] that emits the given optional attribute from the first element in each matched sub-stream
		  */
		def attrOpt[N: AsQName](name: N): Transformer[XmlEvent, Option[String]] = splitter.joinBy(XmlParser.forOptionalAttribute(name))

		/** Consumes each sub-stream with the `XmlParser.forText` parser,
		  * yielding an XmlTransformer[String] which emits a single concatenated String for each sub-stream.
		  *
		  * @return An XmlTransformer[String] that emits the text from each matched sub-stream
		  */
		def text: Transformer[XmlEvent, String] = splitter.joinBy(XmlParser.forText)

		@deprecated("Use `.text` instead", "v0.9")
		def asText = text
	}

	// ----------------------------------------------------------------------------
	// XML ContextMatcher Syntax
	// ----------------------------------------------------------------------------

	/** @group aliases */
	type XmlContextMatcher[+Context] = ContextMatcher[XmlEvent.ElemStart, Context]

	@deprecated("Use `XmlContextMatcher (with lowercase 'ml') instead", "v0.9")
	type XMLContextMatcher[+Context] = XmlContextMatcher[Context]

	/** @group aliases */
	type ElemContextMatcher[+A] = SingleItemContextMatcher[XmlEvent.ElemStart, A]

	/** Context matcher that always matches without consuming any of the tag stack.
	  *
	  * @group contextMatcherSyntax
	  */
	val Root: XmlContextMatcher[Unit] = ContextMatcher.noopSuccess[XmlEvent.ElemStart]

	/** Context matcher that matches any single element at the head of the tag stack.
	  *
	  * @group contextMatcherSyntax
	  */
	val * : ElemContextMatcher[Unit] = SingleItemContextMatcher.predicate("*", _ => true)

	/** Context matcher that matches any number of elements from the head of the tag stack.
	  *
	  * @group contextMatcherSyntax
	  */
	val ** : XmlContextMatcher[Unit] = ContextMatcher.variableLength[XmlEvent.ElemStart]

	/** Context matcher that matches the element at the head of the stack
	  * as long as its name matches the given `elemName`
	  *
	  * This is normally called implicitly, e.g. with `Splitter.xml("foo" \ "bar")`,
	  * but of course can be called explicitly, e.g. `val matcher = elem("foo") \ elem("bar")`
	  *
	  * @param elemName The name of the element.
	  * @tparam N The name *type* - usually `String`, but can be any member of the `[[AsQName]]` typeclass.
	  *           Note that for `javax.xml.namespace.QName` you will need to include the "spac-javax" support library.
	  * @group contextMatcherSyntax
	  */
	implicit def elem[N](elemName: N)(implicit N: AsQName[N]): ElemContextMatcher[Unit] = {
		SingleItemContextMatcher.predicate(
			show"elem(${ AsQName[XmlEvent.ShowableQName].convert(elemName) })",
			{ (e: XmlEvent.ElemStart) => N.equals(elemName, e.qName[N]) }
		)
	}

	/** Context matcher that extracts the (local) name of the element at the head of the stack.
	  * Acts as a convenience for `extractElemQName[String]`
	  *
	  * @group contextMatcherSyntax
	  */
	def extractElemName: ElemContextMatcher[String] = extractElemQName[String]

	/** Context matcher that extracts the (qualified) name of the element at the head of the stack.
	  * The type-level representation of the name is chosen by the caller
	  *
	  * @tparam N The representation of the element's qualified name.
	  *           This could be `String`, but in that case you should use `extractElemName` instead.
	  *           For QName types such as the one from `javax.xml`, you must import a corresponding support package.
	  * @group contextMatcherSyntax
	  */
	def extractElemQName[N: AsQName]: ElemContextMatcher[N] = {
		SingleItemContextMatcher("elem(?)", { (e: XmlEvent.ElemStart) => Some(e.qName[N]) })
	}

	/** Context matcher that extracts the given attribute from the element at the head of the stack.
	  *
	  * @param attrName The name of the attribute to extract
	  * @tparam N The name *type* - usually `String`, but can be any member of the `[[AsQName]]` typeclass.
	  *           Note that for `javax.xml.namespace.QName` you will need to include the "spac-javax" support library.
	  * @group contextMatcherSyntax
	  */
	def attr[N: AsQName](attrName: N): ElemContextMatcher[String] = {
		SingleItemContextMatcher(
			show"attr(${AsQName show attrName})",
			{ (e: XmlEvent.ElemStart) => e.attr(attrName) }
		)
	}

	/** Context matcher that extracts the given optional attribute from the element at the head of the stack.
	  * If the attribute is missing from the head element, this matcher will succeed with a result of None,
	  * as opposed to the `attr` matcher which would fail.
	  *
	  * @param attrName The name of the attribute to extract
	  * @tparam N The name *type* - usually `String`, but can be any member of the `[[AsQName]]` typeclass.
	  *           Note that for `javax.xml.namespace.QName` you will need to include the "spac-javax" support library.
	  * @group contextMatcherSyntax
	  */
	def attrOpt[N: AsQName](attrName: N): ElemContextMatcher[Option[String]] = {
		SingleItemContextMatcher(
			show"attrOpt(${AsQName show attrName})",
			{ (e: XmlEvent.ElemStart) => Some(e.attr(attrName)) }
		)
	}

}
