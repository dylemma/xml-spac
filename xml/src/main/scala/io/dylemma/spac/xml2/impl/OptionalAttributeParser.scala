package io.dylemma.spac.xml2.impl

import cats.Applicative
import io.dylemma.spac.Parser
import io.dylemma.spac.xml2.{AsQName, XmlEvent}

class OptionalAttributeParser[F[+_], N: AsQName](attributeName: N)(implicit F: Applicative[F]) extends Parser[F, XmlEvent, Option[String]] {
	def step(in: XmlEvent): F[Either[Option[String], Parser[F, XmlEvent, Option[String]]]] = in.asElemStart match {
		case None => F.pure(Right(this))
		case Some(elem) => F.pure(Left(elem.attr(attributeName)))
	}
	def finish: F[Option[String]] = F.pure(None)
}
