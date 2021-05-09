package io.dylemma.spac

import cats.data.Chain

/**
  * @param elems
  * @tparam A
  * @group context
  */
case class ContextTrace[+A](elems: Chain[(ContextLocation, A)]) {
	def /[A2 >: A](subContext: ContextTrace[A2]): ContextTrace[A2] = ContextTrace(elems ++ subContext.elems)
	def asSpacTraceElems[A2 >: A] = Chain.fromSeq(elems.reverseIterator.map { case (loc, a) => SpacTraceElement.InInputContext[A2](a, loc) }.toSeq)
}
/**
  * @group context
  */
object ContextTrace {
	def empty: ContextTrace[Nothing] = ContextTrace(Chain.nil)
}

/** A map-like representation of some location in a stream,
  * used like stack trace elements for reporting errors in stream processing.
  *
  * @group context
  */
trait ContextLocation {
	def get[A](tag: ContextLocation.Tag[A]): Option[A]

	protected def tagsForToString: Iterable[ContextLocation.Tag[_]] = List(
		ContextLocation.Tag.LineNumber,
		ContextLocation.Tag.ColumnOffset,
		ContextLocation.Tag.CharOffset,
	)

	override def toString = {
		val entries = for {
			tag <- tagsForToString.iterator
			value <- get(tag)
		} yield s"${tag.name}: $value"

		if (entries.isEmpty) "{ <unknown location> }"
		else entries.mkString("{", ", ", "}")
	}
}

/**
  * @group context
  */
object ContextLocation {
	val empty: ContextLocation = new ContextLocationImpl(Map.empty)

	case class Entry[A](tag: ContextLocation.Tag[A], dim: A)
	def apply(entries: Entry[_]*): ContextLocation = new ContextLocationImpl(entries.view.map(e => (e.tag, e.dim)).toMap)

	private class ContextLocationImpl(val dimensions: Map[ContextLocation.Tag[_], Any]) extends ContextLocation {
		def get[A](tag: ContextLocation.Tag[A]): Option[A] = dimensions.get(tag).map(_.asInstanceOf[A])

		override def tagsForToString: Iterable[Tag[_]] = dimensions.keys
	}

	abstract class Tag[A](val name: String) {
		def ->>(dim: A) = ContextLocation.Entry(this, dim)
	}
	object Tag {
		/** ContextLocation dimension representing the line number of an event within its source */
		case object LineNumber extends Tag[Long]("line")

		/** ContextLocation dimension representing the column offset of an event within its respective line within its source */
		case object ColumnOffset extends Tag[Long]("col")

		/** ContextLocation dimension representing the character offset of an event within its source */
		case object CharOffset extends Tag[Long]("offset")
	}
}
