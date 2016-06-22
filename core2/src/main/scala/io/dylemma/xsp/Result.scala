package io.dylemma.xsp

import scala.util.Try
import scala.util.control.NonFatal

/** Result value from parsing some input.
	* Possible values are Success (with a value), Error (with an exception), or Empty.
	*
	* @tparam T The type of values contained in a `Success`ful result
	*/
sealed trait Result[+T] {
	def map[U](f: T => U): Result[U]
	def withFilter(f: T => Boolean): Result[T]
	def filter(f: T => Boolean) = withFilter(f)
	def flatMap[U](f: T => Result[U]): Result[U]
	def foreach[U](f: T => U): Unit
	def recover[U >: T](f: PartialFunction[Throwable, U]): Result[U]
	def recoverWith[U >: T](f: PartialFunction[Throwable, Result[U]]): Result[U]
	def orElse[U >: T](that: => Result[U]): Result[U]
	def collect[U](pf: PartialFunction[T, U]): Result[U]
	def flatten[U](implicit ev: T <:< Result[U]): Result[U] = flatMap(ev)
	def isEmpty: Boolean
	def isError: Boolean
	def isSuccess: Boolean
	def get: T

	@throws[Throwable]("If this result is an Error, the wrapped exception will be thrown")
	def toOption: Option[T]

	@throws[NoSuchElementException]("If this result is Empty")
	def toTry: Try[T]
}

object Result {
	case object Empty extends Result[Nothing] {
		def map[U](f: Nothing => U): Result[U] = this
		def flatMap[U](f: Nothing => Result[U]): Result[U] = this
		def withFilter(f: Nothing => Boolean): Result[Nothing] = this
		def foreach[U](f: Nothing => U): Unit = ()
		def recover[U >: Nothing](f: PartialFunction[Throwable, U]) = this
		def recoverWith[U >: Nothing](f: PartialFunction[Throwable, Result[U]]) = this
		def orElse[U >: Nothing](that: => Result[U]) = that
		def collect[U](pf: PartialFunction[Nothing, U]) = this
		def isEmpty = true
		def isError = false
		def isSuccess = false
		def get = throw new NoSuchElementException("Empty.get")
		def toOption = None
		def toTry = throw new NoSuchElementException("Empty.toTry")
	}
	case class Error(cause: Throwable) extends Result[Nothing] {
		def map[U](f: Nothing => U): Result[U] = this
		def flatMap[U](f: Nothing => Result[U]): Result[U] = this
		def withFilter(f: Nothing => Boolean): Result[Nothing] = this
		def foreach[U](f: Nothing => U): Unit = ()
		def recover[U >: Nothing](f: PartialFunction[Throwable, U]) = {
			tryDo{
				if(f isDefinedAt cause) Result(f(cause))
				else this
			}
		}
		def recoverWith[U >: Nothing](f: PartialFunction[Throwable, Result[U]]) = {
			tryDo {
				if(f isDefinedAt cause) f(cause)
				else this
			}
		}
		def orElse[U >: Nothing](that: => Result[U]) = that
		def collect[U](pf: PartialFunction[Nothing, U]) = this
		def isEmpty = false
		def isError = true
		def isSuccess = false
		def get = throw cause
		def toOption = throw cause
		def toTry = scala.util.Failure(cause)
	}
	case class Success[T](result: T) extends Result[T] {

		def map[U](f: T => U): Result[U] = tryDo { Success(f(result)) }
		def flatMap[U](f: T => Result[U]): Result[U] = tryDo { f(result) }
		def withFilter(f: T => Boolean): Result[T] = tryDo { if(f(result)) this else Empty }
		def foreach[U](f: T => U): Unit = f(result)
		def recover[U >: T](f: PartialFunction[Throwable, U]) = this
		def recoverWith[U >: T](f: PartialFunction[Throwable, Result[U]]) = this
		def orElse[U >: T](that: => Result[U]) = this
		def collect[U](pf: PartialFunction[T, U]) = tryDo {
			if(pf isDefinedAt result) Success(pf(result))
			else Empty
		}
		def isEmpty = false
		def isError = false
		def isSuccess = true
		def get = result
		def toOption = Some(result)
		def toTry = scala.util.Success(result)
	}
	object Success {
		lazy val unit = Success(())
		lazy val none = Success(None)
	}

	@inline private def tryDo[U](work: => Result[U]): Result[U] = {
		try { work } catch { case NonFatal(err) => Error(err) }
	}

	/** Run some `work` expression, wrapping its result as a `Result`.
		* If an exception is thrown by `work`, it will be caught and this function
		* will return an `Error` containing that exception.
		*/
	def apply[T](work: => T): Result[T] =
		try { Success(work) }
		catch { case NonFatal(err) => Error(err) }

	/** Run the call-by-name `work` expression, catching any thrown exceptions
		* as an `Error`, and mapping actual results to either an `Empty` or a
		* `Success` depending on the returned value.
		*/
	def fromOption[T](work: => Option[T]): Result[T] = apply(work) flatMap {
		case None => Empty
		case Some(value) => Success(value)
	}

	/** Convert a list or `Results` into a single result containing
		* a list of the values in those results. `Empty` results will
		* be filtered out. Any `Error` result will cause the entire
		* computation to become that error.
		*/
	def list[T](results: List[Result[T]]): Result[List[T]] = {
		results.foldRight[Result[List[T]]](Success(Nil)){
			case (Success(t), accum) => accum map { t :: _ }
			case (Empty, accum) => accum
			case (e: Error, _) => e
		}
	}
}