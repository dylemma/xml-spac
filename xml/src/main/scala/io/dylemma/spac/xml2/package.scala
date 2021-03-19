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
	val XmlParser: ParserApplyWithBoundInput[XmlEvent] = Parser.over[XmlEvent]
	/** Type alias for a `Parser` whose input type is `XmlEvent`.
	  *
	  * @group parser
	  */
	type XmlParser[F[+_], Out] = Parser[F, XmlEvent, Out]

	/** @group parser */
	val XmlParserSIO: ParserApplyBound[SyncIO, XmlEvent] = Parser[SyncIO, XmlEvent]
	/** @group parser */
	type XmlParserSIO[Out] = Parser[SyncIO, XmlEvent, Out]

	/** XML-specific parser constructor methods for `XmlParser` or `Parser.over[XmlEvent]`
	  *
	  * @group parser
	  */
	implicit class XmlParserApplyOps(val parserApply: ParserApplyWithBoundInput[XmlEvent]) extends AnyVal {
		def forText[F[+_] : Applicative]: Parser[F, XmlEvent, String] = new XmlTextCollector[F](Chain.nil)
		def forMandatoryAttribute[F[+_], N: AsQName](attributeName: N)(implicit F: MonadError[F, Throwable]): Parser[F, XmlEvent, String] = new MandatoryAttributeParser(attributeName)
		def forOptionalAttribute[F[+_] : Applicative, N: AsQName](attributeName: N): Parser[F, XmlEvent, Option[String]] = new OptionalAttributeParser(attributeName)
	}

	/** XML-specific parser constructor methods for `XmlParser[F]` or `Parser[F, XmlEvent]`
	  *
	  * @group parser
	  */
	implicit class XmlParserApplyFOps[F[+_]](private val parserApply: ParserApplyBound[F, XmlEvent]) extends AnyVal {
		def forText(implicit F: Applicative[F]): Parser[F, XmlEvent, String] = XmlParser.forText
		def forMandatoryAttribute[N: AsQName](attributeName: N)(implicit F: MonadError[F, Throwable]): Parser[F, XmlEvent, String] = XmlParser.forMandatoryAttribute(attributeName)
		def forOptionalAttribute[N: AsQName](attributeName: N)(implicit F: Applicative[F]): Parser[F, XmlEvent, Option[String]] = XmlParser.forOptionalAttribute(attributeName)
	}

	// ----------------------------------------------------------------------------
	// Splitter & XmlSplitter - companion ops
	// ----------------------------------------------------------------------------

	/** @group splitter */
	val XmlSplitter: SplitterApplyWithBoundInput[XmlEvent] = Splitter.over[XmlEvent]
	/** @group splitter */
	type XmlSplitter[F[+_], C] = Splitter[F, XmlEvent, C]

	/** @group splitter */
	val XmlSplitterSIO: SplitterApplyBound[SyncIO, XmlEvent] = Splitter[SyncIO, XmlEvent]
	/** @group splitter */
	type XmlSplitterSIO[C] = Splitter[SyncIO, XmlEvent, C]

	/** @group splitter */
	implicit class XmlSplitterApplyOps(val splitter: Splitter.type) extends AnyVal {
		def xml[F[+_], C](matcher: ContextMatcher[XmlEvent.ElemStart, C])(implicit F: Monad[F]): XmlSplitter[F, C] = splitter.fromMatcher(matcher)
	}
	/** @group splitter */
	implicit class XmlSplitterApplyBoundOps[F[+_]](val splitter: SplitterApplyBound[F, XmlEvent]) {
		def xml[C](matcher: ContextMatcher[XmlEvent.ElemStart, C])(implicit F: Monad[F]): XmlSplitter[F, C] = splitter.fromMatcher(matcher)
	}
	/** @group splitter */
	implicit class XmlSplitterApplyWithBoundEffectOps[F[+_]](val splitter: SplitterApplyWithBoundEffect[F]) {
		// probably the most convenient usage: `Splitter[F].xml(...)`
		def xml[C](matcher: ContextMatcher[XmlEvent.ElemStart, C])(implicit F: Monad[F]): XmlSplitter[F, C] = splitter.fromMatcher(matcher)
	}

	// ----------------------------------------------------------------------------
	// XmlSplitter - member ops
	// ----------------------------------------------------------------------------

	/** @group splitter2 */
	implicit class XmlSplitterOps[F[+_], C](splitter: Splitter[F, XmlEvent, C])(implicit F: MonadError[F, Throwable]) {
		def attr[N: AsQName](name: N): Transformer[F, XmlEvent, String] = splitter.map(_ => XmlParser.forMandatoryAttribute(name))
		def attrOpt[N: AsQName](name: N): Transformer[F, XmlEvent, Option[String]] = splitter.map(_ => XmlParser.forOptionalAttribute(name))
		def text: Transformer[F, XmlEvent, String] = splitter.map(_ => XmlParser.forText)
	}

	/** @group splitter2 */
	implicit class SplitterWordFirstXmlOps[F[+_]](first: Splitter.SplitterWordFirst[F, XmlEvent, Any])(implicit F: MonadError[F, Throwable]) {
		def attr[N: AsQName](name: N): Parser[F, XmlEvent, String] = first.into { _ => XmlParser.forMandatoryAttribute(name) }
		def attrOpt[N: AsQName](name: N): Parser[F, XmlEvent, Option[String]] = first.into { _ => XmlParser.forOptionalAttribute(name) }
		def text: Parser[F, XmlEvent, String] = first.into { _ => XmlParser.forText }
	}

	/** @group splitter2 */
	implicit class SplitterWordFirstOptXmlOps[F[+_]](firstOpt: Splitter.SplitterWordFirstOpt[F, XmlEvent, Any])(implicit F: MonadError[F, Throwable]) {
		def attr[N: AsQName](name: N): Parser[F, XmlEvent, Option[String]] = firstOpt.into { _ => XmlParser.forMandatoryAttribute(name) }
		def attrOpt[N: AsQName](name: N): Parser[F, XmlEvent, Option[Option[String]]] = firstOpt.into { _ => XmlParser.forOptionalAttribute(name) }
		def text: Parser[F, XmlEvent, Option[String]] = firstOpt.into { _ => XmlParser.forText }
	}

	/** @group splitter2 */
	implicit class SplitterWordAsListXmlOps[F[+_]](asList: Splitter.SplitterWordAsList[F, XmlEvent, Any])(implicit F: MonadError[F, Throwable]) {
		def attr[N: AsQName](name: N): Parser[F, XmlEvent, List[String]] = asList.into { _ => XmlParser.forMandatoryAttribute(name) }
		def attrOpt[N: AsQName](name: N): Parser[F, XmlEvent, List[Option[String]]] = asList.into { _ => XmlParser.forOptionalAttribute(name) }
		def text: Parser[F, XmlEvent, List[String]] = asList.into { _ => XmlParser.forText }
	}

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
