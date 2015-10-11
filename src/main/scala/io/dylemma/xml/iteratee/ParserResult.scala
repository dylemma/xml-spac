package io.dylemma.xml.iteratee

/** Result value from parsing some input.
	* Possible values are Success (with a value), Error (with an exception), or Empty.
	*
	* @tparam T The type of values contained in a ``Success`ful result
	*/
sealed trait ParserResult[+T] {
	def map[U](f: T => U): ParserResult[U] = this match {
		case Empty => Empty
		case e: Error => e
		case Success(t) =>
			try { Success(f(t)) }
			catch { case err: Throwable => Error(err) }
	}
	def withFilter(f: T => Boolean): ParserResult[T] = this match {
		case Empty => Empty
		case e: Error => e
		case s @ Success(t) =>
			try { if(f(t)) s else Empty }
			catch { case err: Throwable => Error(err) }
	}
	def flatMap[U](f: T => ParserResult[U]): ParserResult[U] = this match {
		case Empty => Empty
		case e: Error => e
		case Success(t) =>
			try { f(t) }
			catch { case err: Throwable => Error(err) }
	}
	def foreach[U](f: T => U): Unit = this match {
		case Success(t) => f(t)
		case _ => ()
	}
}

case object Empty extends ParserResult[Nothing]
case class Error(cause: Throwable) extends ParserResult[Nothing]
case class Success[T](result: T) extends ParserResult[T]