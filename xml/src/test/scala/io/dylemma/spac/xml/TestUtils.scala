package io.dylemma.spac.xml

import java.io.Writer

import javax.xml.namespace.{NamespaceContext, QName}
import javax.xml.stream.events.{Attribute, Characters, EndElement, StartElement}
import javax.xml.stream.{Location, XMLStreamConstants}
import org.scalatest.Assertions

object TestUtils extends Assertions {
	def mockElem(name: String, attrs: (String, String)*): StartElement = {
		new StartElement {
			val getName: QName = new QName(name)
			def getEventType: Int = XMLStreamConstants.START_ELEMENT
			def getAttributes: java.util.Iterator[_] = null
			def getNamespaces: java.util.Iterator[_] = null
			def getAttributeByName(name: QName): Attribute = attrs.collectFirst { case (key, value) if key == name.getLocalPart => mockAttribute(name, value) }.orNull
			def getNamespaceContext: NamespaceContext = fail("Unexpected call to getNamespaceContext")
			def getNamespaceURI(prefix: String): String = fail("Unexpected call to getNamespaceURI")
			def getLocation: Location = fail("Unexpected call to getLocation")
			def isStartElement: Boolean = true
			def isAttribute: Boolean = false
			def isNamespace: Boolean = false
			def isEndElement: Boolean = false
			def isEntityReference: Boolean = false
			def isProcessingInstruction: Boolean = false
			def isCharacters: Boolean = false
			def isStartDocument: Boolean = false
			def isEndDocument: Boolean = false
			def asStartElement(): StartElement = this
			def asEndElement(): EndElement = null
			def asCharacters(): Characters = null
			def getSchemaType: QName = fail("Unexpected call to getSchemaType")
			def writeAsEncodedUnicode(writer: Writer): Unit = fail("Unexpected call to writeAsEncodedUnicode")
		}
	}

	def mockAttribute(name: QName, value: String): Attribute = new Attribute {
		def getName: QName = name
		def getValue: String = value

		// everything else is unimplemented
		def getDTDType: String = fail("Unexpected call to getDTDType")
		def isSpecified: Boolean = fail("Unexpected call to isSpecified")
		def getEventType: Int = fail("Unexpected call to getEventType")
		def getLocation: Location = fail("Unexpected call to getLocation")
		def isStartElement: Boolean = fail("Unexpected call to isStartElement")
		def isAttribute: Boolean = fail("Unexpected call to isAttribute")
		def isNamespace: Boolean = fail("Unexpected call to isNamespace")
		def isEndElement: Boolean = fail("Unexpected call to isEndElement")
		def isEntityReference: Boolean = fail("Unexpected call to isEntityReference")
		def isProcessingInstruction: Boolean = fail("Unexpected call to isProcessingInstruction")
		def isCharacters: Boolean = fail("Unexpected call to isCharacters")
		def isStartDocument: Boolean = fail("Unexpected call to isStartDocument")
		def isEndDocument: Boolean = fail("Unexpected call to isEndDocument")
		def asStartElement(): StartElement = fail("Unexpected call to asStartElement")
		def asEndElement(): EndElement = fail("Unexpected call to asEndElement")
		def asCharacters(): Characters = fail("Unexpected call to asCharacters")
		def getSchemaType: QName = fail("Unexpected call to getSchemaType")
		def writeAsEncodedUnicode(writer: Writer): Unit = fail("Unexpected call to writeAsEncodedUnicode")
	}
}
