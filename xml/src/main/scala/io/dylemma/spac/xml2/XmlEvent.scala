package io.dylemma.spac.xml2

import cats.Show
import cats.data.Chain
import cats.implicits._
import io.dylemma.spac._

/** @group event */
sealed trait XmlEvent {
	import XmlEvent._
	def asElemStart: Option[ElemStart] = None
	def asElemEnd: Option[ElemEnd] = None
	def asText: Option[Text] = None
	def location: ContextLocation
}

/** @group event */
trait AsXmlEvent[-E] {
	def unapply(event: E): Option[XmlEvent]
}

/** @group event */
object AsXmlEvent {
	implicit val identity: AsXmlEvent[XmlEvent] = new AsXmlEvent[XmlEvent] {
		def unapply(event: XmlEvent): Option[XmlEvent] = Some(event)
	}
}

/** Adapter for various representations of QName
  * @group event
  */
trait AsQName[N] {
	def name(n: N): String
	def namespaceUri(n: N): Option[String]
	def convert[N2: AsQName](from: N2): N
	def equals[N2: AsQName](l: N, r: N2): Boolean
}
/** @group event */
object AsQName {
	def apply[N](implicit instance: AsQName[N]): AsQName[N] = instance

	implicit val stringAsQNameIgnoringNamespace: AsQName[String] = new AsQName[String] {
		def name(n: String): String = n
		def namespaceUri(n: String): Option[String] = None
		def convert[N2](from: N2)(implicit N2: AsQName[N2]): String = N2.name(from)
		def equals[N2](l: String, r: N2)(implicit N2: AsQName[N2]): Boolean = l == N2.name(r)
	}
}

/** @group event */
object XmlEvent {

	// ------------------------------------------------

	object ElemStart {
		def unapply(e: XmlEvent): Option[ElemStart] = e.asElemStart
	}
	trait ElemStart extends XmlEvent {
		def qName[N: AsQName]: N
		def name: String = qName[String]

		def attr[N: AsQName](attributeQName: N): Option[String]
		def attrs[N: AsQName]: Iterator[(N, String)]

		override def toString: String = {
			val sb = new StringBuilder("<")
			sb append show"${qName[ShowableQName]}"
			for((k,v) <- attrs[ShowableQName]) {
				sb append show""" $k="$v""""
			}
			sb append '>'
			sb.result()
		}
		override def asElemStart: Option[ElemStart] = Some(this)
	}

	// ------------------------------------------------

	object ElemEnd {
		def unapply(e: XmlEvent): Option[ElemEnd] = e.asElemEnd
	}
	trait ElemEnd extends XmlEvent {
		def qName[N: AsQName]: N
		def name: String = qName[String]
		override def toString: String = show"</${qName[ShowableQName]}>"
		override def asElemEnd: Option[ElemEnd] = Some(this)
	}

	// ------------------------------------------------

	object Text {
		def unapply(e: XmlEvent): Option[Text] = e.asText
	}
	trait Text extends XmlEvent {
		def value: String
		def isWhitespace: Boolean

		override def toString: String = value
		override def asText: Option[Text] = Some(this)
	}

	// ------------------------------------------------

	/** Concrete QName type used just for toString implementations on events */
	case class ShowableQName(namespaceUri: Option[String], name: String)
	object ShowableQName {
		implicit val asQName: AsQName[ShowableQName] = new AsQName[ShowableQName] {
			def name(n: ShowableQName): String = n.name
			def namespaceUri(n: ShowableQName): Option[String] = n.namespaceUri
			def convert[N2](from: N2)(implicit N2: AsQName[N2]): ShowableQName = ShowableQName(N2.namespaceUri(from), N2.name(from))
			def equals[N2](l: ShowableQName, r: N2)(implicit N2: AsQName[N2]): Boolean = l.namespaceUri == N2.namespaceUri(r) && l.name == N2.name(r)
		}
		implicit val showInstance: Show[ShowableQName] = Show.show {
			case ShowableQName(None, name) => name
			case ShowableQName(Some(uri), name) => s"{$uri}$name"
		}
	}

	// ------------------------------------------------

	implicit val showDebugXmlEvent: Show[XmlEvent] = Show.show[XmlEvent] {
		case ElemStart(e) =>
			val sb = new StringBuilder("ElemStart(")
			sb append show"${e.qName[ShowableQName]}"
			for{ (k, v) <- e.attrs[ShowableQName] } {
				sb append ", "
				sb append k
				sb append '='
				sb append v
			}
			sb append ')'
			sb.result()

		case ElemEnd(e) =>
			show"ElemEnd(${e.qName[ShowableQName]})"

		case Text(e) =>
			if(e.isWhitespace) s"Whitespace(length=${e.value.length})"
			else {
				val chars = e.value.iterator.takeWhile(_ != '\n').take(120).mkString
				val ellipsis = if(chars.length < e.value.length) " [...]" else ""
				s"Text($chars$ellipsis)"
			}

		case _ =>
			// there are only the three main subclasses of XmlEvent, but the compiler is saying
			// the match will fail on ElemEnd(), ElemStart(), Text(), which makes no sense,
			// so this is here just to silence that warning
			""
	}

	// ------------------------------------------------

	/** Interpreter that decides how to treat XmlEvents as ContextPush and ContextPop,
	  * and in what order the context changes should occur relative to their trigger events.
	  */
	implicit val xmlEventStackable: StackLike[XmlEvent, ElemStart] = {
		case start: ElemStart =>
			// ElemStart pushes a context BEFORE being processed
			ContextPush(ContextTrace(Chain.one(start.location -> start)), start).beforeInput

		case _: ElemEnd =>
			// ElemEnd pops from the context AFTER being processed
			ContextPop.afterInput

		case _ => StackInterpretation.NoChange
	}
}
