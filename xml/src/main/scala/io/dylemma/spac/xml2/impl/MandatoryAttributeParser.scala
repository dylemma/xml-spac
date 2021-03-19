package io.dylemma.spac.xml2
package impl

import cats.MonadError
import cats.implicits._
import io.dylemma.spac._
import io.dylemma.spac.xml2.XmlEvent._
import io.dylemma.spac.xml2.{AsQName, XmlEvent}

class MandatoryAttributeParser[F[+_], N, A](attributeName: N)(implicit F: MonadError[F, Throwable], N: AsQName[N]) extends Parser[F, XmlEvent, String] {
	def step(in: XmlEvent): F[Either[String, Parser[F, XmlEvent, String]]] = in.asElemStart match {
		case None => F.pure(Right(this)) // continue searching
		case Some(elem) => elem.attr(attributeName) match {
			case Some(value) => F.pure(Left(value)) // successful result
			case None => F.raiseError {
				// attribute missing from element
				new XmlSpacException.MissingMandatoryAttributeException(AsQName[ShowableQName].convert(attributeName), Some(elem))
			}
		}
	}
	def finish: F[String] = F.raiseError {
		new XmlSpacException.MissingMandatoryAttributeException(AsQName[ShowableQName].convert(attributeName), None)
	}
}

class MissingMandatoryAttributeException(attributeName: ShowableQName, elem: Option[ElemStart]) extends NoSuchElementException(elem match {
	case Some(e) => show"Expected attribute '$attributeName' was missing from $e"
	case None => show"Expected an element with a '$attributeName' attribute, but no element was found"
})