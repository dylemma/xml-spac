package io.dylemma.spac.xml

import cats.effect.SyncIO
import fs2._
import fs2.data.xml.{Attr, QName, referenceResolver, XmlEvent => Fs2XmlEvent}
import io.dylemma.spac.ContextLocation
import io.dylemma.spac.xml.Fs2DataSupport.fs2DataQNameAsQName

private class Fs2StartTagAsElemStart(private val wrapped: Fs2XmlEvent.StartTag) extends XmlEvent.ElemStart {
	def qName[N](implicit N: AsQName[N]): N = N.convert(wrapped.name)

	def attr[N](attributeQName: N)(implicit N: AsQName[N]): Option[String] = {
		wrapped
			.attributes
			.find { attr => N.equals(attributeQName, attr.name) }
			.map { attr => resolveOrThrow(attr.value) }
	}

	def attrs[N](implicit N: AsQName[N]): Iterator[(N, String)] = {
		wrapped
			.attributes
			.iterator
			.map { attr => N.convert(attr.name) -> resolveOrThrow(attr.value) }
	}

	// unfortunately no line/column/offset info is available for fs2-data's XmlEvents
	def location: ContextLocation = ContextLocation.empty

	private def resolveOrThrow(attrValues: List[Fs2XmlEvent.XmlTexty]): String = attrValues match {
		case Fs2XmlEvent.XmlString(s, _) :: Nil =>
			s // already resolved

		case unresolved =>
			// Attempt to run the default `referenceResolver` on a dummy element that holds the unresolved attributes.
			// In theory this should give us a single XmlString to represent the attribute, otherwise we'll throw an exception.
			// Ideally we'll never reach this case, as user should be passing their fs2-data-xml events through a `referenceResolver`
			// before they pass those events into xml-spac.
			val dummyTag = Fs2XmlEvent.StartTag(QName("placeholder"), Attr(QName("placeholder"), unresolved) :: Nil, isEmpty = true)
			val resolvedDummyTag = Stream.emit(dummyTag).through(referenceResolver[SyncIO]()).compile.lastOrError.unsafeRunSync()
			resolvedDummyTag match {
				case Fs2XmlEvent.StartTag(_, resolvedAttrs, _) =>
					resolvedAttrs match {
						case Attr(_, Fs2XmlEvent.XmlString(s, _) :: Nil) :: Nil => s
						case _ => throw new IllegalArgumentException(s"Failed to resolve attribute text events to a single string: $unresolved")
					}
				case _ =>
					// should be an impossible case
					throw new IllegalArgumentException("Backup resolver logic destroyed the attribute placeholder")
			}
	}
}

private class Fs2EndTagAsElemEnd(private val wrapped: Fs2XmlEvent.EndTag) extends XmlEvent.ElemEnd {
	def qName[N](implicit N: AsQName[N]): N = N.convert(wrapped.name)
	def location: ContextLocation = ContextLocation.empty
}

private class Fs2XmlTextyAsText(private val wrapped: Fs2XmlEvent.XmlTexty) extends XmlEvent.Text {
	lazy val value: String = wrapped match {
		case Fs2XmlEvent.XmlString(s, _) => s
		case other =>
			// similar to with attributes, we'd prefer that users pass their XmlEvents through a `referenceResolver`,
			// but if they don't, we'll try to resolve the text value using the default resolver
			Stream
				.emit(other)
				.through(referenceResolver[SyncIO]())
				.collect { case Fs2XmlEvent.XmlString(s, _) => s }
				.compile
				.toList
				.unsafeRunSync()
			match {
				case Nil => throw new IllegalArgumentException(s"Couldn't resolve $other to an XmlString")
				case strings => strings.mkString
			}
	}
	def isWhitespace: Boolean = value.forall(_.isWhitespace)
	def location: ContextLocation = ContextLocation.empty
}