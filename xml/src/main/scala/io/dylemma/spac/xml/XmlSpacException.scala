package io.dylemma.spac
package xml

import cats.data.Chain
import cats.syntax.show._
import io.dylemma.spac.xml.XmlEvent.{ElemStart, ShowableQName}

/** SpacException subtype for XML-specific exceptions
  *
  * @group aliases
  */
trait XmlSpacException[Self <: XmlSpacException[Self]] extends SpacException[Self]

/** Contains the actual `XmlSpacException` subtypes
  *
  * @group aliases
  */
object XmlSpacException {

	def missingMandatoryAttribute[N: AsQName](attributeName: N, elem: Option[ElemStart]) = new MissingMandatoryAttributeException(AsQName[ShowableQName].convert(attributeName), elem, Chain.nil)
	class MissingMandatoryAttributeException(val attributeName: ShowableQName, val elem: Option[ElemStart], spacTrace: Chain[SpacTraceElement])
		extends SpacException[MissingMandatoryAttributeException](spacTrace, showMissingAttr(attributeName, elem))
	{
		def withSpacTrace(spacTrace2: Chain[SpacTraceElement]) = new MissingMandatoryAttributeException(attributeName, elem, spacTrace2)
	}
	object MissingMandatoryAttributeException {
		def unapply(e: Throwable) = e match {
			case mmae: MissingMandatoryAttributeException => Some(mmae.attributeName, mmae.elem)
			case _ => None
		}
	}

	private def showMissingAttr(attributeName: ShowableQName, elem: Option[ElemStart]) = elem match {
		case Some(e) => show"Expected attribute '$attributeName' was missing from $e"
		case None => show"Expected an element with a '$attributeName' attribute, but no element was found"
	}
}