package io.dylemma.spac.old.xml.syntax

import javax.xml.namespace.QName
import javax.xml.stream.events.StartElement
import io.dylemma.spac.{ContextMatcher, SingleItemContextMatcher}

import scala.language.implicitConversions

/** Defines XML-specific conveniences for creating `ContextMatchers`. */
trait ContextMatcherSyntax {

	/** Context matcher that always matches without consuming any of the tag stack.
	  */
	val Root = ContextMatcher.noopSuccess[StartElement]

	/** Context matcher that matches any single element at the head of the tag stack.
	  */
	val * = SingleItemContextMatcher.predicate("*", { _: StartElement => true })

	val ** = ContextMatcher.variableLength[StartElement]

	/** Context matcher that matches the element at the head of the stack
	  * as long as its name is equal to the given `qname`.
	  *
	  * @param qname The required name (QName) for the element at the head of the stack
	  */
	def elem(qname: QName) = SingleItemContextMatcher.predicate(s"elem($qname)", { e: StartElement => e.getName == qname })

	/** Context matcher that matches the element at the head of the stack
	  * as long as its name (local part only) is equal to the given `name`
	  *
	  * @param name The required (local) name for the element at the head of the stack
	  */
	def elem(name: String) = SingleItemContextMatcher.predicate[StartElement](s"elem($name)", { e: StartElement => e.getName.getLocalPart == name })

	/** Context matcher that extracts the (local) name of the element at the head of the stack.
	  */
	val extractElemName = SingleItemContextMatcher("elem(?)", { e: StartElement => Some(e.getName.getLocalPart) })

	/** Context matcher that extracts the (qualified) name of the element at the head of the stack.
	  */
	val extractElemQName = SingleItemContextMatcher("elem(?:?)", { e: StartElement => Some(e.getName) })

	/** Implicitly convert a `String` to an `elem` matcher */
	implicit def stringToElemMatcher(name: String) = elem(name)

	/** Implicitly convert a `QName` to an `elem` matcher */
	implicit def qnameToElemMatcher(qname: QName) = elem(qname)

	/** Context matcher that extracts the given attribute from the element at the head of the stack.
	  *
	  * @param qname The qualified name of the attribute to extract
	  */
	def attr(qname: QName) = SingleItemContextMatcher(
		s"attr($qname)", { e: StartElement => Option(e getAttributeByName qname).map(_.getValue) }
	)

	/** Context matcher that extracts the given attribute from the element at the head of the stack.
	  *
	  * @param name The local name of the attribute to extract
	  */
	def attr(name: String): SingleItemContextMatcher[StartElement, String] = attr(new QName(name))
}
