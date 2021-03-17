package io.dylemma.spac

import cats.data.NonEmptyList
import org.tpolecat.typename._

trait SpacException extends Exception

object SpacException {
	class MissingFirstException[Out: TypeName]
		extends NoSuchElementException(s"No ${ typeName[Out] } was encountered before the end of its input.")
			with SpacException

	class UnexpectedInputException[A](val input: A, val expectations: NonEmptyList[String])
		extends IllegalArgumentException(s"Unexpected input: expected ${ formatExpectations(expectations) }, but got $input")
			with SpacException

	class UnfulfilledInputsException(val expectations: NonEmptyList[String])
		extends IllegalStateException(s"Unexpected end of input: still expected ${ formatExpectations(expectations) }")
			with SpacException

	private def formatExpectations(expectations: NonEmptyList[String]) = expectations.tail match {
		case Nil => expectations.head
		case list => (expectations.head :: list).mkString("[", ", then ", "]")
	}

	class FallbackChainFailure(val underlyingErrors: NonEmptyList[Throwable])
		extends Exception("Every parser in the fallback chain failed")
			with SpacException {
		for (err <- underlyingErrors.toList) this.addSuppressed(err)
	}

	object ContextualizedException {
		def apply[In](contextTrace: ContextTrace[In], cause: Throwable): Throwable = {
			if (contextTrace.elems.isEmpty) cause
			else new ContextualizedException(contextTrace, cause)
		}
		private def formatContext[A](contextTrace: ContextTrace[A]) = {
			// private constructor guarded by companion object's `apply` method ensures `contextTrace` is non-empty
			contextTrace.elems.iterator
				.map { case (loc, in) => s"\t@ $loc - $in" }
				.mkString(s"Exception thrown from parsing logic: [\n", "\n", "\n]")
		}
	}
	class ContextualizedException[In] private(val contextTrace: ContextTrace[In], cause: Throwable)
		extends Exception(ContextualizedException.formatContext(contextTrace), cause)
			with SpacException
}