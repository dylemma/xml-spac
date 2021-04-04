package io.dylemma.spac

/** A play on words vs StackTraceElement, a *Spac* trace element represents some contextual location inside the logic of a spac Parser,
  * or the location of an input to that parser.
  * `SpacTraceElement`s are used by `SpacException` to provide useful debugging information for when a Parser fails.
  */
trait SpacTraceElement {
	def render: String

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
	case class InParse(methodName: String, callerPos: util.Pos) extends SpacTraceElement {
		def render = {
			s"$methodName - ${callerPos.render}"
		}
	}

	/** Indicates the usage of a splitter, and the source location that constructed that splitter.
	  * Generally, the `splitterNote` will be the `.toString` of whichever ContextMatcher was used to construct the splitter.
	  */
	case class InSplitter(splitterNote: String, pos: util.Pos) extends SpacTraceElement {
		def render = {
			s"Splitter(${truncateNote(splitterNote)}) - ${pos.render}"
		}
	}

	/** The top of a typical SpacException's `spacTrace`, representing whichever raw input caused the parser to throw.
	  * If the input is an instance of `HasLocation`, the location will be displayed as part of the `render` of this element.
	  * `XmlEvent` and `JsonEvent` (from `xml-spac` and `json-spac` respectively) both extend HasLocation,
	  */
	case class InInput[A](input: A) extends SpacTraceElement {
		def render = {
			val location = input match {
				case a: HasLocation => a.location
				case _ => ContextLocation.empty
			}
			s"Input(${truncateNote(formatInput(input))}) - $location"
		}
	}

	/** The top of a typical SpacException's `spacTrace`, used when a parser handler's `finish()` throws an exception.
	  * Unlike `InInput`, this element does not contain location information.
	  */
	case object AtInputEnd extends SpacTraceElement {
		def render = {
			"InputEnd"
		}
	}

	/** Used when ContextMatcher-based splitters are involved, appearing in the middle of a SpacException's `spacTrace`.
	  * Similar to `InInput`, but generally represents a previous input which was interpreted as a "stack push".
	  * For example, an XML start-element event would indicate "the error is somewhere within this element"
	  */
	case class InInputContext[A](input: A, location: ContextLocation) extends SpacTraceElement {
		def render = {
			s"InputContext(${truncateNote(formatInput(input))}) - $location"
		}
	}

	private def formatInput(input: Any) = {
		val s = input.toString
		if (s.forall(_.isWhitespace)) "(whitespace only)"
		else s.filterNot(c => c == '\n' || c == '\r').trim
	}

	/** Used when a parser inside a "compound" parser throws an exception, used to indicate which member of the compound threw.
	  * Compound parsers are created via the use of Parser's `Applicative`, e.g. `(parser1, parser2).tupled`.
	  */
	case class InCompound(memberIndex: Int, numMembers: Int) extends SpacTraceElement {
		def render = s"Compound Parser member ${memberIndex + 1} of $numMembers"
	}

}