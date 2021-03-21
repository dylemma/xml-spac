package io.dylemma.spac

import cats.data.Chain

// ------------------------------------------------------

case class ContextTrace[+A](elems: Chain[(ContextLocation, A)]) {
	def /[A2 >: A](subContext: ContextTrace[A2]): ContextTrace[A2] = ContextTrace(elems ++ subContext.elems)
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
}

// ------------------------------------------------------

abstract class ContextLocationTag[A](val name: String)

// ------------------------------------------------------

case object ContextLineNumber extends ContextLocationTag[Long]("lineNumber")

case object ContextColumnOffset extends ContextLocationTag[Long]("columnOffset")

case object ContextCharOffset extends ContextLocationTag[Long]("charOffset")

