package io.dylemma.spac.xml2

import cats.{Applicative, MonadError}
import cats.data.Chain
import impl._
import io.dylemma.spac.Parser

object XmlParser {
	def apply[F[+_]] = new XmlParserApplyWithBoundEffect[F]

	def forText[F[+_]: Applicative]: Parser[F, XmlEvent, String] = new XmlTextCollector[F](Chain.nil)
	def forMandatoryAttribute[F[+_], N: AsQName](attributeName: N)(implicit F: MonadError[F, Throwable]): Parser[F, XmlEvent, String] = new MandatoryAttributeParser(attributeName)
	def forOptionalAttribute[F[+_]: Applicative, N: AsQName](attributeName: N): Parser[F, XmlEvent, Option[String]] = new OptionalAttributeParser(attributeName)

}

class XmlParserApplyWithBoundEffect[F[+_]] {
	def forText(implicit F: Applicative[F]): Parser[F, XmlEvent, String] = XmlParser.forText
	def forMandatoryAttribute[N: AsQName](attributeName: N)(implicit F: MonadError[F, Throwable]): Parser[F, XmlEvent, String] = XmlParser.forMandatoryAttribute(attributeName)
	def forOptionalAttribute[N: AsQName](attributeName: N)(implicit F: Applicative[F]): Parser[F, XmlEvent, Option[String]] = new OptionalAttributeParser(attributeName)
}