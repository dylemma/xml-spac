package io.dylemma.spac.util

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

case class Pos(filename: String, line: Int) {
	def render = s"$filename:$line"
}

object Pos {
	implicit def capture: Pos = macro locationMacro

	def locationMacro(c: blackbox.Context) = {
		import c.universe._

		val filename = c.enclosingPosition.source.file.name
		val line = c.enclosingPosition.line

		q"_root_.io.dylemma.spac.util.Pos($filename, $line)"
	}
}
