package io.dylemma.spac

/** A play on words vs StackTraceElement, a *Spac* trace element represents some contextual location inside the logic of a spac Parser,
  * or the location of an input to that parser.
  * `SpacTraceElement`s are used by `SpacException` to provide useful debugging information for when a Parser fails.
  */
trait SpacTraceElement {
	/** Create a String that somewhat resembles a StackTraceElement, without actually conforming to StackTraceElement's `toString`.
	  */
	def render: String

	/** Create a StackTraceElement, abusing the `className/methodName/fileName` arguments such that
	  * the created StackTraceElement's `toString` returns something similar to the `render` method.
	  *
	  * The general pattern is that SpacTraceElements that include `CallerPos` arguments will use
	  * either `parser` or `client` as the "className" and provide a real fileName and line number.
	  * Input-related events will use "input" as the "className" and "data source" as the fileName.
	  *
	  * @return A StackTraceElement that resembles this SpacTraceElement's `render` output
	  */
	def toStackTraceElement: StackTraceElement

	protected def truncateNote(note: String, maxLen: Int = 100) = {
		val truncated = "[...truncated]"
		val _maxLen = maxLen - truncated.length
		if (note.length > _maxLen) note.take(_maxLen) + truncated else note
	}
}

object SpacTraceElement {
	/** The bottom of a typical SpacException's `spacTrace`, representing the specific `parse` method
	  * that was called in order to run the parser, and the location of the caller.
	  */
	case class InParse(methodName: String, callerPos: CallerPos) extends SpacTraceElement {
		def render = {
			s"$methodName - ${callerPos.render}"
		}
		def toStackTraceElement = new StackTraceElement("parser", methodName, callerPos.filename, callerPos.line)
	}

	/** Indicates the usage of a splitter, and the source location that constructed that splitter.
	  * Generally, the `splitterNote` will be the `.toString` of whichever ContextMatcher was used to construct the splitter.
	  */
	case class InSplitter(splitterNote: String, pos: CallerPos) extends SpacTraceElement {
		def render = {
			s"Splitter(${truncateNote(splitterNote)}) - ${pos.render}"
		}
		def toStackTraceElement = new StackTraceElement("client", s"splitter(${truncateNote(splitterNote)}) ", pos.filename, pos.line)
	}

	/** The top of a typical SpacException's `spacTrace`, representing whichever raw input caused the parser to throw.
	  * If the input is an instance of `HasLocation`, the location will be displayed as part of the `render` of this element.
	  * `XmlEvent` and `JsonEvent` (from `xml-spac` and `json-spac` respectively) both extend HasLocation,
	  */
	case class InInput[A](input: A) extends SpacTraceElement {
		def location = input match {
			case a: HasLocation => a.location
			case _ => ContextLocation.empty
		}
		def render = {
			s"Input(${truncateNote(formatInput(input))}) - $location"
		}
		def toStackTraceElement = new StackTraceElement("input", s"event(${truncateNote(formatInput(input))}) @ $location ", "data source", -1)
	}

	/** The top of a typical SpacException's `spacTrace`, used when a parser handler's `finish()` throws an exception.
	  * Unlike `InInput`, this element does not contain location information.
	  */
	case object AtInputEnd extends SpacTraceElement {
		def render = {
			"InputEnd"
		}
		def toStackTraceElement = new StackTraceElement("input", "eof ", "data source", -1)
	}

	/** Used when an error occurs in an underlying parser (e.g. javax or jackson)
	  * to indicate the location of the last successfully-parsed token.
	  * Happens for example when parsing invalid XML or JSON.
	  *
	  * @param location
	  */
	case class NearLocation(location: ContextLocation) extends SpacTraceElement {
		def render = {
			s"Near($location)"
		}
		def toStackTraceElement = new StackTraceElement("input", s"nearby($location) ", "data source", -1)
	}

	/** Used when ContextMatcher-based splitters are involved, appearing in the middle of a SpacException's `spacTrace`.
	  * Similar to `InInput`, but generally represents a previous input which was interpreted as a "stack push".
	  * For example, an XML start-element event would indicate "the error is somewhere within this element"
	  */
	case class InInputContext[A](input: A, location: ContextLocation) extends SpacTraceElement {
		def render = {
			s"InputContext(${truncateNote(formatInput(input))}) - $location"
		}
		def toStackTraceElement = new StackTraceElement("input", s"context(${truncateNote(formatInput(input))}) @ $location ", "data source", -1)
	}

	private def formatInput(input: Any) = {
		val s = input.toString
		if (s.forall(_.isWhitespace)) "(whitespace only)"
		else s.filterNot(c => c == '\n' || c == '\r').trim
	}

	/** Used when a parser inside a "compound" parser throws an exception, used to indicate which member of the compound threw.
	  * Compound parsers are created via the use of Parser's `Applicative`, e.g. `(parser1, parser2).tupled`.
	  */
	case class InCompound(memberIndex: Int, numMembers: Int, callerPos: CallerPos) extends SpacTraceElement {
		def render = s"Compound Parser member ${memberIndex + 1} of $numMembers"
		def toStackTraceElement = new StackTraceElement("parser", s"compound member ${memberIndex + 1} of $numMembers ", callerPos.filename, callerPos.line)
	}

}