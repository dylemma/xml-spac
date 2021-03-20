package io.dylemma.spac

import cats.data.Chain
import cats.effect.SyncIO
import cats.syntax.show._
import cats.{Applicative, Monad, MonadError}
import io.dylemma.spac.xml2.impl._

import scala.language.implicitConversions

/**
  * @groupname parser XML Parser
  * @groupname splitter XML Splitter
  * @groupname splitter2 XML Splitter extension methods
  * @groupname contextMatcherSyntax XML Context Matcher Construction
  * @groupname event XML Event Representation
  * @groupprio parser 0
  * @groupprio splitter 1
  * @groupprio splitter2 2
  * @groupprio contextMatcherSyntax 3
  * @groupprio event 4
  */
package object xml2 {

	// ----------------------------------------------------------------------------
	// XmlParser - companion ops
	// ----------------------------------------------------------------------------

	/** Like the `Parser` companion object, but only for creating Parsers whose input type is `XmlEvent`.
	  *
	  * @see [[XmlParserApplyOps]]
	  * @group parser
	  */
	val XmlParser: ParserApplyWithBoundInput[XmlEvent] = Parser[XmlEvent]
	/** Type alias for a `Parser` whose input type is `XmlEvent`.
	  *
	  * @group parser
	  */
	type XmlParser[Out] = Parser[XmlEvent, Out]

	@deprecated("Use `XmlParser` (with lowercase 'ml') to reference the parser companion", "v0.9")
	val XMLParser = XmlParser
	@deprecated("Use `XmlParser` (with lowercase 'ml') instead", "v0.9")
	type XMLParser[Out] = XmlParser[Out]

	/** XML-specific parser constructor methods for `XmlParser` or `Parser.over[XmlEvent]`
	  *
	  * @group parser
	  */
	implicit class XmlParserApplyOps(val parserApply: ParserApplyWithBoundInput[XmlEvent]) extends AnyVal {
		def forText: Parser[XmlEvent, String] = XmlParserText
		def forMandatoryAttribute[N: AsQName](attributeName: N): Parser[XmlEvent, String] = new XmlParserMandatoryAttribute(attributeName)
		def forOptionalAttribute[N: AsQName](attributeName: N): Parser[XmlEvent, Option[String]] = new XmlParserOptionalAttribute(attributeName)
		def attr[N: AsQName](attributeName: N): XmlParser[String] = forMandatoryAttribute(attributeName)
		def attrOpt[N: AsQName](attributeName: N): XmlParser[Option[String]] = forOptionalAttribute(attributeName)
	}

	// ----------------------------------------------------------------------------
	// Splitter & XmlSplitter - companion ops
	// ----------------------------------------------------------------------------

	/** @group splitter */
	val XmlSplitter: SplitterApplyWithBoundInput[XmlEvent] = Splitter[XmlEvent]
	/** @group splitter */
	type XmlSplitter[C] = Splitter[XmlEvent, C]

	@deprecated("Use `XmlSplitter` (with lowercase 'ml') for directly referencing the companion object, or use `Splitter.xml` to construct a new XmlSplitter", "v0.9")
	val XMLSplitter = XmlSplitter
	@deprecated("Use `XmlSplitter` (with lowercase 'ml') instead", "v0.9")
	type XMLSplitter[C] = XmlSplitter[C]
	/** @group splitter */
	implicit class XmlSplitterApplyOps(val splitter: Splitter.type) extends AnyVal {
		def xml[C](matcher: ContextMatcher[XmlEvent.ElemStart, C]): XmlSplitter[C] = splitter.fromMatcher(matcher)
	}

	// ----------------------------------------------------------------------------
	// XmlSplitter - member ops
	// ----------------------------------------------------------------------------

	/** @group splitter2 */
	implicit class XmlSplitterOps[C](splitter: Splitter[XmlEvent, C]) {
		def attr[N: AsQName](name: N): Transformer[XmlEvent, String] = splitter.map(_ => XmlParser.forMandatoryAttribute(name))
		def attrOpt[N: AsQName](name: N): Transformer[XmlEvent, Option[String]] = splitter.map(_ => XmlParser.forOptionalAttribute(name))
		def text: Transformer[XmlEvent, String] = splitter.map(_ => XmlParser.forText)

		@deprecated("Use `.text` instead", "v0.9")
		def asText = text
	}

//	/** @group splitter2 */
//	implicit class SplitterWordFirstXmlOps(first: Splitter.SplitterWordFirst[XmlEvent, Any]) {
//		def attr[N: AsQName](name: N): Parser[XmlEvent, String] = first.map { _ => XmlParser.forMandatoryAttribute(name) }
//		def attrOpt[N: AsQName](name: N): Parser[XmlEvent, Option[String]] = first.map { _ => XmlParser.forOptionalAttribute(name) }
//		def text: Parser[XmlEvent, String] = first.map { _ => XmlParser.forText }
//	}
//
//	/** @group splitter2 */
//	implicit class SplitterWordFirstOptXmlOps(firstOpt: Splitter.SplitterWordFirstOpt[XmlEvent, Any]) {
//		def attr[N: AsQName](name: N): Parser[XmlEvent, Option[String]] = firstOpt.map { _ => XmlParser.forMandatoryAttribute(name) }
//		def attrOpt[N: AsQName](name: N): Parser[XmlEvent, Option[Option[String]]] = firstOpt.map { _ => XmlParser.forOptionalAttribute(name) }
//		def text: Parser[XmlEvent, Option[String]] = firstOpt.map { _ => XmlParser.forText }
//	}
//
//	/** @group splitter2 */
//	implicit class SplitterWordAsListXmlOps(asList: Splitter.SplitterWordAsList[XmlEvent, Any]) {
//		def attr[N: AsQName](name: N): Parser[XmlEvent, List[String]] = asList.map { _ => XmlParser.forMandatoryAttribute(name) }
//		def attrOpt[N: AsQName](name: N): Parser[XmlEvent, List[Option[String]]] = asList.map { _ => XmlParser.forOptionalAttribute(name) }
//		def text: Parser[XmlEvent, List[String]] = asList.map { _ => XmlParser.forText }
//	}

	// ----------------------------------------------------------------------------
	// XML ContextMatcher Syntax
	// ----------------------------------------------------------------------------

	/** @group contextMatcherSyntax */
	type StackContextMatcher[+A] = ContextMatcher[XmlEvent.ElemStart, A]
	/** @group contextMatcherSyntax */
	type ElemContextMatcher[+A] = SingleItemContextMatcher[XmlEvent.ElemStart, A]

	/** @group contextMatcherSyntax */
	val Root: StackContextMatcher[Unit] = ContextMatcher.noopSuccess[XmlEvent.ElemStart]

	/** @group contextMatcherSyntax */
	val * : ElemContextMatcher[Unit] = SingleItemContextMatcher.predicate("*", _ => true)
	/** @group contextMatcherSyntax */
	val ** : StackContextMatcher[Unit] = ContextMatcher.variableLength[XmlEvent.ElemStart]

	/** @group contextMatcherSyntax */
	implicit def elem[N](elemName: N)(implicit N: AsQName[N]): ElemContextMatcher[Unit] = {
		SingleItemContextMatcher.predicate(
			show"elem(${ AsQName[XmlEvent.ShowableQName].convert(elemName) })",
			{ e: XmlEvent.ElemStart => N.equals(elemName, e.qName[N]) }
		)
	}

	/** @group contextMatcherSyntax */
	def extractElemName: ElemContextMatcher[String] = extractElemQName[String]
	/** @group contextMatcherSyntax */
	def extractElemQName[N: AsQName]: ElemContextMatcher[N] = {
		SingleItemContextMatcher("elem(?)", { e: XmlEvent.ElemStart => Some(e.qName[N]) })
	}

	/** @group contextMatcherSyntax */
	def attr[N: AsQName](attrName: N): ElemContextMatcher[String] = {
		SingleItemContextMatcher(
			show"attr(${ AsQName[XmlEvent.ShowableQName].convert(attrName) })",
			{ e: XmlEvent.ElemStart => e.attr(attrName) }
		)
	}

}
