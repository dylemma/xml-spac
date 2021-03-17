package io.dylemma.spac
package xml2

import cats.MonadError

object XmlExtensions extends XmlExtensions
trait XmlExtensions {

	implicit class XmlSplitterOps[F[+_], C](splitter: Splitter[F, XmlEvent, C])(implicit F: MonadError[F, Throwable]) {
		def attr[N: AsQName](name: N): Transformer[F, XmlEvent, String] = splitter.map(_ => XmlParser.forMandatoryAttribute(name))
		def attrOpt[N: AsQName](name: N): Transformer[F, XmlEvent, Option[String]] = splitter.map(_ => XmlParser.forOptionalAttribute(name))
		def text: Transformer[F, XmlEvent, String] = splitter.map(_ => XmlParser.forText)
	}

	implicit class SplitterWordFirstXmlOps[F[+_]](first: Splitter.SplitterWordFirst[F, XmlEvent, Any])(implicit F: MonadError[F, Throwable]) {
		def attr[N: AsQName](name: N): Parser[F, XmlEvent, String] = first.into { _ => XmlParser.forMandatoryAttribute(name) }
		def attrOpt[N: AsQName](name: N): Parser[F, XmlEvent, Option[String]] = first.into { _ => XmlParser.forOptionalAttribute(name) }
		def text: Parser[F, XmlEvent, String] = first.into { _ => XmlParser.forText }
	}

	implicit class SplitterWordFirstOptXmlOps[F[+_]](firstOpt: Splitter.SplitterWordFirstOpt[F, XmlEvent, Any])(implicit F: MonadError[F, Throwable]) {
		def attr[N: AsQName](name: N): Parser[F, XmlEvent, Option[String]] = firstOpt.into { _ => XmlParser.forMandatoryAttribute(name) }
		def attrOpt[N: AsQName](name: N): Parser[F, XmlEvent, Option[Option[String]]] = firstOpt.into { _ => XmlParser.forOptionalAttribute(name) }
		def text: Parser[F, XmlEvent, Option[String]] = firstOpt.into { _ => XmlParser.forText }
	}

	implicit class SplitterWordAsListXmlOps[F[+_]](asList: Splitter.SplitterWordAsList[F, XmlEvent, Any])(implicit F: MonadError[F, Throwable]) {
		def attr[N: AsQName](name: N): Parser[F, XmlEvent, List[String]] = asList.into { _ => XmlParser.forMandatoryAttribute(name) }
		def attrOpt[N: AsQName](name: N): Parser[F, XmlEvent, List[Option[String]]] = asList.into { _ => XmlParser.forOptionalAttribute(name) }
		def text: Parser[F, XmlEvent, List[String]] = asList.into { _ => XmlParser.forText }
	}
}
