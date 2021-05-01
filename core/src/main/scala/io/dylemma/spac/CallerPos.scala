package io.dylemma.spac

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

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

/**
  * @group errors
  */
object CallerPos {
	implicit def capture: CallerPos = macro locationMacro

	def locationMacro(c: blackbox.Context) = {
		import c.universe._

		val filename = c.enclosingPosition.source.file.name
		val line = c.enclosingPosition.line

		q"_root_.io.dylemma.spac.CallerPos($filename, $line)"
	}
}
