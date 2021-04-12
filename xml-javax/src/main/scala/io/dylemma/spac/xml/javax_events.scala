package io.dylemma.spac.xml

import io.dylemma.spac.ContextLocation
import io.dylemma.spac.xml.JavaxSupport.javaxQNameAsQName
import io.dylemma.spac.xml.XmlEvent._
import javax.xml.namespace.QName

private class ElemStartImpl(
	_qName: QName,
	attrsMap: collection.Map[String, List[(QName, String)]],
	val location: ContextLocation
) extends ElemStart {

	def qName[N](implicit N: AsQName[N]) = N.convert(_qName)
	def attr[N](attributeQName: N)(implicit N: AsQName[N]) = {
		attrsMap.get(N.name(attributeQName)).flatMap(_.collectFirst {
			case (qname, v) if N.equals(attributeQName, qname) => v
		})
	}
	def attrs[N](implicit N: AsQName[N]): Iterator[(N, String)] = {
		for {
			(baseName, entries) <- attrsMap.iterator
			(qName, value) <- entries.iterator
		} yield {
			N.convert(qName) -> value
		}
	}
}

private class ElemEndImpl(_qName: QName, val location: ContextLocation) extends ElemEnd {
	def qName[N](implicit N: AsQName[N]) = N.convert(_qName)
}

private class XmlTextImpl(
	val value: String,
	val isWhitespace: Boolean,
	val location: ContextLocation
) extends Text