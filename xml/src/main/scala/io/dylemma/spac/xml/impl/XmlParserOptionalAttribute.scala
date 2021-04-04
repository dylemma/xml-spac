package io.dylemma.spac
package xml
package impl

class XmlParserOptionalAttribute[N: AsQName](attributeName: N) extends Parser.Stateless[XmlEvent, Option[String]] {
	def step(in: XmlEvent) = in.asElemStart match {
		case Some(elem) => Left(elem.attr(attributeName))
		case None => Right(this)
	}
	def finish() = None
	override def toString = s"XmlParser.attrOpt(${AsQName.show(attributeName)})"
}
