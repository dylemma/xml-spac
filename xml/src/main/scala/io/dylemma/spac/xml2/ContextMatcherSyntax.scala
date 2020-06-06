package io.dylemma.spac.xml2

import cats.implicits._
import io.dylemma.spac.xml2.XmlEvent.{ElemStart, ShowableQName}
import io.dylemma.spac.{ContextMatcher, SingleItemContextMatcher}

import scala.language.implicitConversions

object ContextMatcherSyntax extends ContextMatcherSyntax
trait ContextMatcherSyntax {

	type StackContextMatcher[+A] = ContextMatcher[ElemStart, A]
	type ElemContextMatcher[+A] = SingleItemContextMatcher[ElemStart, A]

	val Root: StackContextMatcher[Unit] = ContextMatcher.noopSuccess[ElemStart]

	val * : ElemContextMatcher[Unit] = SingleItemContextMatcher.predicate("*", _ => true)
	val ** : StackContextMatcher[Unit] = ContextMatcher.variableLength[ElemStart]

	implicit def elem[N](elemName: N)(implicit N: AsQName[N]): ElemContextMatcher[Unit] = {
		SingleItemContextMatcher.predicate(
			show"elem(${AsQName[ShowableQName].convert(elemName)})",
			{ e: ElemStart => N.equals(elemName, e.qName[N]) }
		)
	}

	def extractElemName: ElemContextMatcher[String] = extractElemQName[String]
	def extractElemQName[N: AsQName]: ElemContextMatcher[N] = {
		SingleItemContextMatcher("elem(?)", { e: ElemStart => Some(e.qName[N]) })
	}

	def attr[N: AsQName](attrName: N): ElemContextMatcher[String] = {
		SingleItemContextMatcher(
			show"attr(${AsQName[ShowableQName].convert(attrName)})",
			{ e: ElemStart => e.attr(attrName) }
		)
	}
}
