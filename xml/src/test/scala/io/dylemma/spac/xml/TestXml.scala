package io.dylemma.spac.xml

import io.dylemma.spac.ContextLocation
import io.dylemma.spac.xml2.{AsQName, XmlEvent}

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/** This is a stupidly simple XML Parser based on the scala parser combinators library.
  * Don't try to use this for any production purposes.
  * It's just here to make it easier to make xml parser tests in the absence of a specific XML integration.
  *
  * E.g.
  * {{{
  *    testXml"<foo>Hello</foo>"
  * }}}
  */
object TestXml {

	implicit class TestXmlStringContext(sc: StringContext) {
		def testXml(): List[XmlEvent] = {
			val raw = StringContext.processEscapes(sc.parts.mkString.stripMargin)
			Parsing.parseAll(Parsing.root, raw) getOrElse {
				throw new IllegalArgumentException("couldn't parse the string as xml")
			}
		}
	}

	def testElem(name: String, attrs: (String, String)*): XmlEvent.ElemStart = TestStartElem(name, attrs.toList)
	def testQName(localPart: String): XmlEvent.ShowableQName = XmlEvent.ShowableQName(None, localPart)


	case class TestStartElem(_name: String, _attrs: List[(String, String)]) extends XmlEvent.ElemStart {
		def qName[N](implicit N: AsQName[N]): N = N.convert(_name)
		def attr[N](attributeQName: N)(implicit N: AsQName[N]): Option[String] = _attrs.collectFirst {
			case (name, value) if N.equals(attributeQName, name) => value
		}
		def attrs[N](implicit N: AsQName[N]): Iterator[(N, String)] = _attrs.iterator.map {
			case (name, value) => N.convert(name) -> value
		}
		def location: ContextLocation = ContextLocation.empty
	}
	case class TestEndElem(_name: String) extends XmlEvent.ElemEnd {
		def qName[N](implicit N: AsQName[N]): N = N.convert(_name)
		def location: ContextLocation = ContextLocation.empty
	}
	case class TestXmlChars(s: String) extends XmlEvent.Text {
		def value: String = s
		def isWhitespace: Boolean = s.forall(_.isWhitespace)
		def location: ContextLocation = ContextLocation.empty
	}

	private object Parsing extends RegexParsers {
		override protected val whiteSpace = "".r

		val name = """[a-zA-Z][a-zA-Z0-9_-]*""".r
		val string = "\"" ~> """[^"]*""".r <~ "\""
		val spaces = "\\s+".r.?

		val startElemStart = "<" ~> name
		val attribute = (spaces ~> name <~ '=') ~ string ^^ { case name ~ value => (name, value) }

		val endElemStart1 = spaces ~> ">" ^^ { _ => (name: String, attrs: List[(String, String)]) => TestStartElem(name, attrs) :: Nil }
		val endElemStart2 = spaces ~> "/>" ^^ { _ => (name: String, attrs: List[(String, String)]) => TestStartElem(name, attrs) :: TestEndElem(name) :: Nil }
		val endElemStart = endElemStart1 | endElemStart2

		val elemStart = startElemStart ~ attribute.* ~ endElemStart ^^ { case name ~ attrs ~ finish => finish(name, attrs) }

		val charsOutsideElem = """[^<]+""".r ^^ { s => TestXmlChars(s) :: Nil }

		val endElem = "</" ~> name <~ ">" ^^ { TestEndElem(_) :: Nil }

		val root = elemStart ~ (elemStart | charsOutsideElem | endElem).* ^^ { case start ~ tails => (start :: tails).flatten }
	}

}
