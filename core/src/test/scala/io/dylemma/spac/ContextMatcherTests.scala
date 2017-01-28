package io.dylemma.spac

import javax.xml.stream.XMLStreamException
import javax.xml.stream.events.{Attribute, StartElement, XMLEvent}

import collection.JavaConverters._
import org.scalatest.{FunSpec, Matchers}

import scala.util.{Failure, Success, Try}

class ContextMatcherTests extends FunSpec with Matchers {

	/** Intermediate class to represent a `StartElement` event */
	protected case class Elem(name: String, attrs: Map[String, String] = Map.empty){

		def toScalaXml(next: List[Elem]): scala.xml.Elem = {
			val child = next match {
				case Nil => Seq.empty
				case head :: tail => Seq(head toScalaXml tail)
			}
			scala.xml.Elem(null, name, attrsToMetadata(attrs.toList), scala.xml.TopScope, true, child: _*)
		}
	}

	protected def attrsToMetadata(attrs: List[(String, String)]): scala.xml.MetaData = attrs match {
		case Nil => scala.xml.Null
		case (key, value) :: tail => scala.xml.Attribute(null, key, value, attrsToMetadata(tail))
	}

	/** Convert a `StartElement` to an `Elem`, pretty much just as a sanity check */
	protected def eventToElem(event: StartElement) = {
		val name = event.getName.getLocalPart
		val attrs = for {
			attrAny <- event.getAttributes.asScala
		} yield {
			val attr = attrAny.asInstanceOf[Attribute]
			attr.getName.getLocalPart -> attr.getValue
		}
		Elem(name, attrs.toMap)
	}

	/** Because mocking the XMLEvent class is a pain, we'll use our custom Elem class
		* as the "mock", and this method to convert them to actual StartElement events
		* by parsing source generated from them. Obviously there will be a test to make
		* sure this operation works as expected.
		* @param source
		* @return
		*/
	protected def events(source: Elem*): List[StartElement] = {
		val strSource = source.toList match {
			case Nil => ""
			case head :: tail => (head toScalaXml tail).toString
		}
		val consumer = Transformer
			.Collect[XMLEvent, StartElement]{ case e: StartElement => e }
		  .take(source.size)
		  .consumeToList
		consumer.consume(strSource)
	}

	describe("event mocking"){
		it("should be sane"){
			val elems = Elem("a", Map("foo" -> "bar")) :: Elem("b") :: Elem("c", Map("argle" -> "bargle")) :: Nil
			events(elems: _*).map(eventToElem) should be(elems)
		}
	}

	// TODO: test the ContextMatchers, especially the new ** one
}
