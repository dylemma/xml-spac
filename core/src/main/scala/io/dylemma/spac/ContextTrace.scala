package io.dylemma.spac

import cats.data.Chain

// ------------------------------------------------------

case class ContextTrace[+A](elems: Chain[(ContextLocation, A)]) {
	def /[A2 >: A](subContext: ContextTrace[A2]): ContextTrace[A2] = ContextTrace(elems ++ subContext.elems)
	def asSpacTraceElems[A2 >: A] = Chain.fromSeq(elems.reverseIterator.map { case (loc, a) => SpacTraceElement.InInputContext[A2](a, loc) }.toSeq)
}
object ContextTrace {
	def empty: ContextTrace[Nothing] = ContextTrace(Chain.nil)
}

/** A map-like representation of some location in a stream,
  * used like stack trace elements for reporting errors in stream processing.
  */
class ContextLocation private[ContextLocation](val dimensions: Map[ContextLocationTag[_], Any]) {
	def and[A](tag: ContextLocationTag[A], dim: A): ContextLocation = {
		new ContextLocation(dimensions.updated(tag, dim))
	}
	def get[A](tag: ContextLocationTag[A]): Option[A] = dimensions.get(tag).map(_.asInstanceOf[A])

	override def toString = {
		if (dimensions.isEmpty) "{ <unknown location> }"
		else dimensions.view.map { case (k, v) => s"${k.name}: $v" }.mkString("{", ", ", "}")
	}
}
object ContextLocation {
	def empty: ContextLocation = new ContextLocation(Map.empty)
	def of[A](tag: ContextLocationTag[A], dim: A): ContextLocation = new ContextLocation(Map(tag -> dim))

	case class Entry[A](tag: ContextLocationTag[A], dim: A)
	def apply(entries: Entry[_]*): ContextLocation = new ContextLocation(entries.view.map(e => (e.tag, e.dim)).toMap)
}

// ------------------------------------------------------

abstract class ContextLocationTag[A](val name: String) {
	def ->>(dim: A) = ContextLocation.Entry(this, dim)
}

// ------------------------------------------------------

case object ContextLineNumber extends ContextLocationTag[Long]("line")

case object ContextColumnOffset extends ContextLocationTag[Long]("col")

case object ContextCharOffset extends ContextLocationTag[Long]("offset")

