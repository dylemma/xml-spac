//package io.dylemma.xml
//
//import javax.xml.namespace.QName
//import javax.xml.stream.events.XMLEvent
//import event._
//
///**
// * Created by Dylan on 5/28/2015.
// */
//case class StaxContext(openTags: List[(QName, Attributes)])
//
//trait StAXContextMatcher extends (StaxContext => Boolean)
//
//object StAXContextDSL {
//
//	object Root {
//		def \(tagName: String) = SimpleContextMatcher(tagName :: Nil)
//	}
//
//	case class SimpleContextMatcher(expectedTags: List[String]) extends StAXContextMatcher {
//		def apply(context: StaxContext) = context.openTags.corresponds(expectedTags) {
//			case ((Name(tag), _), expected) => tag == expected
//		}
//
//		def \(tagName: String) = SimpleContextMatcher(expectedTags :+ tagName)
//	}
//
//	trait EventCollector[A] {
//		def handle(event: XMLEvent): Unit
//		def enterContext(context: StaxContext): Unit
//		def exitContext(context: StaxContext): Unit
//		def state: A
//	}
//
//	class TextEventCollector extends EventCollector[String] {
//		private val sb = new StringBuilder
//		var state = ""
//		def handle(event: XMLEvent) = event match {
//			case Characters(text) if text.trim.nonEmpty => sb append text
//			case _ => ()
//		}
//		def enterContext(context: StaxContext) = sb.clear()
//		def exitContext(context: StaxContext) = {
//			state = sb.result()
//			sb.clear()
//		}
//	}
//
//	trait EventFolder[A] {
//		def init: A
//		def fold(state: A, event: XMLEvent): A
//	}
//
//	case class TextFolder() extends EventFolder[StringBuilder] {
//		def init = new StringBuilder
//		def fold(sb: StringBuilder, event: XMLEvent) = event match {
//			case Characters(text) if text.trim.nonEmpty => sb append text
//			case _ => sb
//		}
//	}
//}
