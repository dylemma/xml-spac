package io.dylemma.spac
package xml2

import cats.syntax.show._
import io.dylemma.spac.xml2.XmlEvent.{ElemStart, ShowableQName}

trait XmlSpacException extends SpacException
object XmlSpacException {

	class MissingMandatoryAttributeException(val attributeName: ShowableQName, val elem: Option[ElemStart])
		extends NoSuchElementException(showMissingAttr(attributeName, elem))
			with XmlSpacException

	private def showMissingAttr(attributeName: ShowableQName, elem: Option[ElemStart]) = elem match {
		case Some(e) => show"Expected attribute '$attributeName' was missing from $e"
		case None => show"Expected an element with a '$attributeName' attribute, but no element was found"
	}
}