package io.dylemma.xml

import scala.util.control.NonFatal

/** Result value from parsing some input.
	* Possible values are Success (with a value), Error (with an exception), or Empty.
	*
	* @tparam T The type of values contained in a `Success`ful result
	*/
sealed trait Result[+T] {
	def map[U](f: T => U): Result[U]
	def withFilter(f: T => Boolean): Result[T]
	def flatMap[U](f: T => Result[U]): Result[U]
	def foreach[U](f: T => U): Unit
}

object Result {
	case object Empty extends Result[Nothing] {
		def map[U](f: Nothing => U): Result[U] = this
		def flatMap[U](f: Nothing => Result[U]): Result[U] = this
		def withFilter(f: Nothing => Boolean): Result[Nothing] = this
		def foreach[U](f: Nothing => U): Unit = ()
	}
	case class Error(cause: Throwable) extends Result[Nothing] {
		def map[U](f: Nothing => U): Result[U] = this
		def flatMap[U](f: Nothing => Result[U]): Result[U] = this
		def withFilter(f: Nothing => Boolean): Result[Nothing] = this
		def foreach[U](f: Nothing => U): Unit = ()
	}
	case class Success[T](result: T) extends Result[T] {
		@inline private def tryDo[U](work: => Result[U]): Result[U] = {
			try { work } catch { case NonFatal(err) => Error(err) }
		}
		def map[U](f: T => U): Result[U] = tryDo { Success(f(result)) }
		def flatMap[U](f: T => Result[U]): Result[U] = tryDo { f(result) }
		def withFilter(f: T => Boolean): Result[T] = tryDo { if(f(result)) this else Empty }
		def foreach[U](f: T => U): Unit = f(result)
	}
}