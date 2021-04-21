package io.dylemma.spac
package json

import cats.Show
import cats.data.Chain

/** ADT for tokens in a JSON stream.
  */
sealed trait JsonEvent extends HasLocation {
	import JsonEvent._

	def isObjectStart: Boolean = false
	def isObjectEnd: Boolean = false
	def isArrayStart: Boolean = false
	def isArrayEnd: Boolean = false
	def asFieldStart: Option[FieldStart] = None
	def asFieldEnd: Option[FieldEnd] = None
	def asIndexStart: Option[IndexStart] = None
	def asIndexEnd: Option[IndexEnd] = None

	def asStackPush: Option[JsonStackElem] = None
	def asStackPop: Option[JsonStackPop] = None
	def asValueEvent: Option[JsonValueEvent] = None

	def asBool: Option[JBool] = None
	def asLong: Option[JLong] = None
	def asDouble: Option[JDouble] = None
	def asString: Option[JString] = None
	def asNull: Option[JNull] = None

	def showRawJson: String
}

/** Subset of JsonEvents that constitute a "context stack push". */
sealed trait JsonStackElem extends JsonEvent {
	override def asStackPush = Some(this)
}

/** Subset of JsonEvents that constitute a "context stack pop". */
sealed trait JsonStackPop extends JsonEvent {
	override def asStackPop = Some(this)
}

/** Subset of JsonEvents that represent a primitive values */
sealed trait JsonValueEvent extends JsonEvent {
	def valueAsString: String
	override def asValueEvent = Some(this)
}

object JsonEvent {

	implicit val jsonStackLike: StackLike[JsonEvent, JsonStackElem] = (e: JsonEvent) => {
		e.asStackPush.map { push =>
			if (push.isObjectStart || push.isArrayStart) {
				// `{` and `[` count as part of the context that they open
				ContextPush(ContextTrace(Chain.one(e.location -> e)), push).beforeInput
			} else {
				// index and field starts are excluded from contexts that they open
				ContextPush(ContextTrace(Chain.one(e.location -> e)), push).afterInput
			}
		} orElse e.asStackPop.map { pop =>
			if (pop.isObjectEnd || pop.isArrayEnd) {
				// `}` and `]` count as part of the context that they close
				ContextPop.afterInput
			} else {
				// index and field ends are excluded from the contexts that they close
				ContextPop.beforeInput
			}
		} getOrElse {
			StackInterpretation.NoChange
		}
	}

	trait ObjectStart extends JsonStackElem {
		override def isObjectStart = true
		override def toString = "ObjectStart"
		def showRawJson = "{"
	}
	object ObjectStart {
		def unapply(e: JsonEvent): Boolean = e.isObjectStart
		def apply(loc: ContextLocation): ObjectStart = new Impl(loc)

		private class Impl(val location: ContextLocation) extends ObjectStart
	}

	trait ObjectEnd extends JsonStackPop {
		override def isObjectEnd = true
		override def toString = "ObjectEnd"
		def showRawJson = "}"
	}
	object ObjectEnd {
		def unapply(e: JsonEvent): Boolean = e.isObjectEnd
		def apply(loc: ContextLocation): ObjectEnd = new Impl(loc)

		private class Impl(val location: ContextLocation) extends ObjectEnd
	}

	trait FieldStart extends JsonStackElem {
		def fieldName: String
		override def asFieldStart = Some(this)
		override def toString = s"FieldStart($fieldName)"
		def showRawJson = s"$fieldName:"
	}
	object FieldStart {
		def unapply(e: JsonEvent): Option[String] = e.asFieldStart.map(_.fieldName)
		def apply(fieldName: String, loc: ContextLocation): FieldStart = new Impl(fieldName, loc)

		private class Impl(val fieldName: String, val location: ContextLocation) extends FieldStart
	}

	trait FieldEnd extends JsonStackPop {
		def fieldName: String
		override def asFieldEnd = Some(this)
		override def toString = s"FieldEnd($fieldName)"
		def showRawJson = ""
	}
	object FieldEnd {
		def unapply(e: JsonEvent): Option[String] = e.asFieldEnd.map(_.fieldName)
		def apply(fieldName: String, loc: ContextLocation): FieldEnd = new Impl(fieldName, loc)

		private class Impl(val fieldName: String, val location: ContextLocation) extends FieldEnd
	}

	trait ArrayStart extends JsonStackElem {
		override def isArrayStart = true
		override def toString = "ArrayStart"
		def showRawJson = "["
	}
	object ArrayStart {
		def unapply(e: JsonEvent): Boolean = e.isArrayStart
		def apply(loc: ContextLocation): ArrayStart = new Impl(loc)

		private class Impl(val location: ContextLocation) extends ArrayStart
	}

	trait ArrayEnd extends JsonStackPop {
		override def isArrayEnd = true
		override def toString = "ArrayEnd"
		def showRawJson = "]"
	}
	object ArrayEnd {
		def unapply(e: JsonEvent): Boolean = e.isArrayEnd
		def apply(loc: ContextLocation): ArrayEnd = new Impl(loc)

		private class Impl(val location: ContextLocation) extends ArrayEnd
	}

	trait IndexStart extends JsonStackElem {
		def index: Int
		override def asIndexStart = Some(this)
		override def toString = s"IndexStart($index)"
		def showRawJson = ""
	}
	object IndexStart {
		def unapply(e: JsonEvent): Option[Int] = e.asIndexStart.map(_.index)
		def apply(index: Int, loc: ContextLocation): IndexStart = new Impl(index, loc)

		private class Impl(val index: Int, val location: ContextLocation) extends IndexStart
	}

	trait IndexEnd extends JsonStackPop {
		def index: Int
		override def asIndexEnd = Some(this)
		override def toString = s"IndexEnd($index)"
		def showRawJson = ""
	}
	object IndexEnd {
		def unapply(e: JsonEvent): Option[Int] = e.asIndexEnd.map(_.index)
		def apply(index: Int, loc: ContextLocation): IndexEnd = new Impl(index, loc)

		private class Impl(val index: Int, val location: ContextLocation) extends IndexEnd
	}

	trait JBool extends JsonValueEvent {
		def booleanValue: Boolean
		def valueAsString = String.valueOf(booleanValue)
		override def asBool = Some(this)
		override def toString = s"JBool($booleanValue)"
		def showRawJson = valueAsString
	}
	object JBool {
		def unapply(e: JsonEvent): Option[Boolean] = e.asBool.map(_.booleanValue)
		def apply(b: Boolean, loc: ContextLocation): JBool = new Impl(b, loc)

		private class Impl(val booleanValue: Boolean, val location: ContextLocation) extends JBool
	}

	trait JLong extends JsonValueEvent {
		def longValue: Long
		def valueAsString = String.valueOf(longValue)
		override def asLong = Some(this)
		override def toString = s"JLong($longValue)"
		def showRawJson = valueAsString
	}
	object JLong {
		def unapply(e: JsonEvent): Option[Long] = e.asLong.map(_.longValue)
		def apply(n: Long, loc: ContextLocation): JLong = new Impl(n, loc)

		private class Impl(val longValue: Long, val location: ContextLocation) extends JLong
	}

	trait JDouble extends JsonValueEvent {
		def doubleValue: Double
		def valueAsString = String.valueOf(doubleValue)
		override def asDouble = Some(this)
		override def toString = s"JDouble($doubleValue)"
		def showRawJson = valueAsString
	}
	object JDouble {
		def unapply(e: JsonEvent): Option[Double] = e.asDouble.map(_.doubleValue)
		def apply(n: Double, loc: ContextLocation): JDouble = new Impl(n, loc)

		private class Impl(val doubleValue: Double, val location: ContextLocation) extends JDouble
	}

	trait JString extends JsonValueEvent {
		def stringValue: String
		def valueAsString = stringValue
		override def asString = Some(this)
		override def toString = s"JString($stringValue)"
		def showRawJson = stringValue.replaceAll("\"", "\\\"")
	}
	object JString {
		def unapply(e: JsonEvent): Option[String] = e.asString.map(_.stringValue)
		def apply(s: String, loc: ContextLocation): JString = new Impl(s, loc)

		private class Impl(val stringValue: String, val location: ContextLocation) extends JString
	}

	trait JNull extends JsonValueEvent {
		def valueAsString = "null"
		override def asNull = Some(this)
		override def toString = "JNull"
		def showRawJson = "null"
	}
	object JNull {
		def unapply(e: JsonEvent): Boolean = e.asNull.isDefined
		def apply(loc: ContextLocation): JNull = new Impl(loc)

		private class Impl(val location: ContextLocation) extends JNull
	}

	implicit val showJsonEventAsRawJson: Show[JsonEvent] = Show.show[JsonEvent] { _.showRawJson }
}
