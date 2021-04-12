package io.dylemma.spac
package json

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
	def isFieldEnd: Boolean = false
	def asIndexStart: Option[IndexStart] = None
	def isIndexEnd: Boolean = false

	def asStackPush: Option[JsonStackElem] = None
	def asStackPop: Option[JsonStackPop] = None
	def asValueEvent: Option[JsonValueEvent] = None

	def asBool: Option[JBool] = None
	def asLong: Option[JLong] = None
	def asDouble: Option[JDouble] = None
	def asString: Option[JString] = None
	def asNull: Option[JNull] = None
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
	override def toString = valueAsString
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
		override def toString = "{"
	}
	object ObjectStart {
		def unapply(e: JsonEvent) = e.isObjectStart
		def apply(loc: ContextLocation): ObjectStart = new Impl(loc)

		private class Impl(val location: ContextLocation) extends ObjectStart
	}

	trait ObjectEnd extends JsonStackPop {
		override def isObjectEnd = true
		override def toString = "}"
	}
	object ObjectEnd {
		def unapply(e: JsonEvent) = e.isObjectEnd
		def apply(loc: ContextLocation): ObjectEnd = new Impl(loc)

		private class Impl(val location: ContextLocation) extends ObjectEnd
	}

	trait FieldStart extends JsonStackElem {
		def fieldName: String
		override def asFieldStart = Some(this)
		override def toString = s"$fieldName:"
	}
	object FieldStart {
		def unapply(e: JsonEvent): Option[FieldStart] = e.asFieldStart
		def apply(fieldName: String, loc: ContextLocation): FieldStart = new Impl(fieldName, loc)

		private class Impl(val fieldName: String, val location: ContextLocation) extends FieldStart
	}

	trait FieldEnd extends JsonStackPop {
		override def isFieldEnd = true
		override def toString = "</field>"
	}
	object FieldEnd {
		def unapply(e: JsonEvent) = e.isFieldEnd
		def apply(loc: ContextLocation): FieldEnd = new Impl(loc)

		private class Impl(val location: ContextLocation) extends FieldEnd
	}

	trait ArrayStart extends JsonStackElem {
		override def isArrayStart = true
		override def toString = "["
	}
	object ArrayStart {
		def unapply(e: JsonEvent) = e.isArrayStart
		def apply(loc: ContextLocation): ArrayStart = new Impl(loc)

		private class Impl(val location: ContextLocation) extends ArrayStart
	}

	trait ArrayEnd extends JsonStackPop {
		override def isArrayEnd = true
		override def toString = "]"
	}
	object ArrayEnd {
		def unapply(e: JsonEvent) = e.isArrayEnd
		def apply(loc: ContextLocation): ArrayEnd = new Impl(loc)

		private class Impl(val location: ContextLocation) extends ArrayEnd
	}

	trait IndexStart extends JsonStackElem {
		def index: Int
		override def asIndexStart = Some(this)
		override def toString = s"<index $index>"
	}
	object IndexStart {
		def unapply(e: JsonEvent) = e.asIndexStart
		def apply(index: Int, loc: ContextLocation): IndexStart = new Impl(index, loc)

		private class Impl(val index: Int, val location: ContextLocation) extends IndexStart
	}

	trait IndexEnd extends JsonStackPop {
		override def isIndexEnd = true
		override def toString = "</index>"
	}
	object IndexEnd {
		def unapply(e: JsonEvent) = e.isIndexEnd
		def apply(loc: ContextLocation): IndexEnd = new Impl(loc)

		private class Impl(val location: ContextLocation) extends IndexEnd
	}

	trait JBool extends JsonValueEvent {
		def booleanValue: Boolean
		def valueAsString = String.valueOf(booleanValue)
		override def asBool = Some(this)
	}
	object JBool {
		def unapply(e: JsonEvent) = e.asBool
		def apply(b: Boolean, loc: ContextLocation): JBool = new Impl(b, loc)

		private class Impl(val booleanValue: Boolean, val location: ContextLocation) extends JBool
	}

	trait JLong extends JsonValueEvent {
		def longValue: Long
		def valueAsString = String.valueOf(longValue)
		override def asLong = Some(this)
	}
	object JLong {
		def unapply(e: JsonEvent) = e.asLong
		def apply(n: Long, loc: ContextLocation): JLong = new Impl(n, loc)

		private class Impl(val longValue: Long, val location: ContextLocation) extends JLong
	}

	trait JDouble extends JsonValueEvent {
		def doubleValue: Double
		def valueAsString = String.valueOf(doubleValue)
		override def asDouble = Some(this)
	}
	object JDouble {
		def unapply(e: JsonEvent) = e.asDouble
		def apply(n: Double, loc: ContextLocation): JDouble = new Impl(n, loc)

		private class Impl(val doubleValue: Double, val location: ContextLocation) extends JDouble
	}

	trait JString extends JsonValueEvent {
		def stringValue: String
		def valueAsString = stringValue
		override def asString = Some(this)
	}
	object JString {
		def unapply(e: JsonEvent) = e.asString
		def apply(s: String, loc: ContextLocation): JString = new Impl(s, loc)

		private class Impl(val stringValue: String, val location: ContextLocation) extends JString
	}

	trait JNull extends JsonValueEvent {
		def valueAsString = "null"
		override def asNull = Some(this)
	}
	object JNull {
		def unapply(e: JsonEvent) = e.asNull
		def apply(loc: ContextLocation): JNull = new Impl(loc)

		private class Impl(val location: ContextLocation) extends JNull
	}

}
