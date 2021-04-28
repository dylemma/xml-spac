package io.dylemma.spac.xml

import io.dylemma.spac.ContextLocation

object TestXml {

	def testElem(name: String, attrs: (String, String)*): XmlEvent.ElemStart = TestStartElem(name, attrs.toList)
	def testQName(localPart: String): XmlEvent.ShowableQName = XmlEvent.ShowableQName(None, localPart)
	def testText(s: String): XmlEvent.Text = TestXmlChars(s)

	private case class TestStartElem(_name: String, _attrs: List[(String, String)]) extends XmlEvent.ElemStart {
		def qName[N](implicit N: AsQName[N]): N = N.convert(_name)
		def attr[N](attributeQName: N)(implicit N: AsQName[N]): Option[String] = _attrs.collectFirst {
			case (name, value) if N.equals(attributeQName, name) => value
		}
		def attrs[N](implicit N: AsQName[N]): Iterator[(N, String)] = _attrs.iterator.map {
			case (name, value) => N.convert(name) -> value
		}
		def location: ContextLocation = ContextLocation.empty
	}
	private case class TestEndElem(_name: String) extends XmlEvent.ElemEnd {
		def qName[N](implicit N: AsQName[N]): N = N.convert(_name)
		def location: ContextLocation = ContextLocation.empty
	}
	private case class TestXmlChars(s: String) extends XmlEvent.Text {
		def value: String = s
		def isWhitespace: Boolean = s.forall(_.isWhitespace)
		def location: ContextLocation = ContextLocation.empty
	}

}
