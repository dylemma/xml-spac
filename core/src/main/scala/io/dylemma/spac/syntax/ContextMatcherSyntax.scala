package io.dylemma.spac.syntax

import javax.xml.namespace.QName
import javax.xml.stream.events.StartElement

import io.dylemma.spac._

import scala.language.implicitConversions

trait ContextMatcherSyntax {

	/** Context matcher that always matches without consuming any of the tag stack.
		*/
	object Root extends ChainingContextMatcher[Unit, Start] {
		protected def applyChain(stack: IndexedSeq[StartElement], offset: Int, length: Int) = Some(Start -> 0)
		protected val minStackLength = Some(0)
		protected val chainRep = ChainRep.UnitChainRep
	}

	/** Context matcher that matches any single element at the head of the tag stack.
		*/
	val * = SingleElementContextMatcher.predicate("*", {_ => true})

	object ** extends ChainingContextMatcher[Unit, Start] {
		protected def applyChain(stack: IndexedSeq[StartElement], offset: Int, length: Int): Option[(Start, Int)] = Some(Start -> 0)
		protected def minStackLength: Option[Int] = Some(0)
		protected def chainRep: ChainRep[Unit, Start] = ChainRep.UnitChainRep
		/** Chain this matcher with the `next` matcher, returning a new matcher which represents the chain.
			*
			* @param next    The next matcher in the chain
			* @param concat  An implicitly-available object that knows how to combine the chain representations
			*                of this matcher and the `next` matcher
			* @param thatRep An implicitly-available object that serves as the `chainRep` for the resulting parser
			* @tparam B         The result type of the `next` parser
			* @tparam BChain    The chain representation of the `next` parser's result type
			* @tparam That      The result type of the combined parser
			* @tparam ThatChain The chain representation of the combined parser's result type
			* @return A new matcher which combines this matcher and the `next` in a chain
			*/
		override def \[B, BChain <: Chain, That, ThatChain <: Chain](
			next: ChainingContextMatcher[B, BChain]
		)(
			implicit concat: ChainConcat[Start, BChain, ThatChain],
			thatRep: ChainRep[That, ThatChain]
		): ChainingContextMatcher[That, ThatChain] = {
			Root \ next.lookDeeper
		}
	}

	/** Context matcher that matches the element at the head of the stack
		* as long as its name is equal to the given `qname`.
 *
		* @param qname The required name (QName) for the element at the head of the stack
		*/
	def elem(qname: QName) = SingleElementContextMatcher.predicate(s"elem($qname)", _.getName == qname)

	/** Context matcher that matches the element at the head of the stack
		* as long as its name (local part only) is equal to the given `name`
 *
		* @param name The required (local) name for the element at the head of the stack
		*/
	def elem(name: String) = SingleElementContextMatcher.predicate(s"elem($name)", _.getName.getLocalPart == name)

	/** Context matcher that extracts the (local) name of the element at the head of the stack.
		*/
	val extractElemName = SingleElementContextMatcher("elem(?)", { e => Some(e.getName.getLocalPart) })

	/** Context matcher that extracts the (qualified) name of the element at the head of the stack.
		*/
	val extractElemQName = SingleElementContextMatcher("elem(?:?)", { e => Some(e.getName) })

	/** Implicitly convert a `String` to an `elem` matcher */
	implicit def stringToElemMatcher(name: String) = elem(name)

	/** Implicitly convert a `QName` to an `elem` matcher */
	implicit def qnameToElemMatcher(qname: QName) = elem(qname)

	/** Context matcher that extracts the given attribute from the element at the head of the stack.
 *
		* @param qname The qualified name of the attribute to extract
		*/
	def attr(qname: QName) = SingleElementContextMatcher(
		s"attr($qname)",
		{ e => Option(e getAttributeByName qname).map(_.getValue) }
	)

	/** Context matcher that extracts the given attribute from the element at the head of the stack.
 *
		* @param name The local name of the attribute to extract
		*/
	def attr(name: String): SingleElementContextMatcher[String, Start ~ String] = attr(new QName(name))
}
