package io.dylemma.spac

/** Since tests related to SpacTraceElements  are sensitive to line numbers,
  * this utility helps us capture the line number of whatever subject we're trying to test,
  * so we can compare line numbers against a "cell" instead of a hard-coded int,
  * and not worry about breaking tests when we add comments or imports
  */
class LineNumberCell {
	var valueOpt: Option[Int] = None
	def &[A](rhs: => A)(implicit pos: CallerPos) = {
		valueOpt = Some(pos.line)
		rhs
	}
	def &:[A](lhs: => A)(implicit pos: CallerPos) = {
		valueOpt = Some(pos.line)
		lhs
	}

	def unapply(pos: CallerPos): Boolean = valueOpt match {
		case Some(line) => line == pos.line
		case _ => false
	}
}
