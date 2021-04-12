package io.dylemma.spac
package xml
package impl

class XmlParserMandatoryAttribute[N: AsQName](attributeName: N) extends Parser.Stateless[XmlEvent, String] {
	def step(in: XmlEvent) = in.asElemStart match {
		case Some(elem) =>
			elem.attr(attributeName) match {
				case Some(value) => Left(value)
				case None => throw XmlSpacException.missingMandatoryAttribute(attributeName, Some(elem))
			}
		case None =>
			Right(this)
	}
	def finish() = {
		throw XmlSpacException.missingMandatoryAttribute(attributeName, None)
	}
	override def toString = s"XmlParser.attr(${AsQName.show(attributeName)})"
}
