package io.dylemma.spac
package xml

import cats.syntax.show._
import io.dylemma.spac.xml.XmlEvent.{ElemStart, ShowableQName}

/** SpacException subtype for XML-specific exceptions
  *
  * @group aliases
  */
trait XmlSpacException extends SpacException

/** Contains the actual `XmlSpacException` subtypes
  *
  * @group aliases
  */
object XmlSpacException {

	class MissingMandatoryAttributeException(val attributeName: ShowableQName, val elem: Option[ElemStart])
		extends NoSuchElementException(showMissingAttr(attributeName, elem))
			with XmlSpacException

	private def showMissingAttr(attributeName: ShowableQName, elem: Option[ElemStart]) = elem match {
		case Some(e) => show"Expected attribute '$attributeName' was missing from $e"
		case None => show"Expected an element with a '$attributeName' attribute, but no element was found"
	}
}