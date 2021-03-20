package io.dylemma.spac
package xml2
package impl

import io.dylemma.spac.xml2.XmlEvent.ShowableQName

class XmlParserMandatoryAttribute[N: AsQName](attributeName: N) extends Parser.Stateless[XmlEvent, String] {
	def step(in: XmlEvent) = in.asElemStart match {
		case Some(elem) =>
			elem.attr(attributeName) match {
				case Some(value) => Left(value)
				case None => throw new XmlSpacException.MissingMandatoryAttributeException(AsQName[ShowableQName].convert(attributeName), Some(elem))
			}
		case None =>
			Right(this)
	}
	def finish() = {
		throw new XmlSpacException.MissingMandatoryAttributeException(AsQName[ShowableQName].convert(attributeName), None)
	}
}
