package io.dylemma.spac

import scala.quoted.{ Expr, Quotes }

/** Represents a location in code that called a method.
  * An implicit instance of this class will be automatically derived by a macro on-demand.
  * CallerPos's ultimate purpose is to be present in certain `SpacTraceElement` classes,
  * helping to point to specific splitters or `parse` calls in the event of a parsing error.
  *
  * @group errors
  */
case class CallerPos(filename: String, line: Int) {
	def render = s"$filename:$line"
}
object CallerPos {
	inline given CallerPos = ${ deriveCallerPos }

	def deriveCallerPos(using quotes: Quotes): Expr[CallerPos] = {
		import quotes.reflect._
		val pos = Position.ofMacroExpansion
		val filename = Expr { pos.sourceFile.jpath.getFileName.toString }
		// note: scala 3 uses 0-based line numbers - https://github.com/lampepfl/dotty/discussions/12728
		val line = Expr { pos.startLine + 1 }
		'{ CallerPos(${filename}, ${line}) }
	}
}