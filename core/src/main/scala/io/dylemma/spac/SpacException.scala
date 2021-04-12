package io.dylemma.spac

import cats.data.Chain
import org.tpolecat.typename._

import scala.util.control.NoStackTrace

/** Base class for all exceptions thrown by Spac parsers.
  * A `SpacException` holds a `spacTrace`, which is similar to a *stack* trace, but uses a specialized element type
  * to hold helpful debug information about the cause and context of the exception, and the input that caused it.
  *
  * SpacException uses `NoStackTrace` to suppress the usual stack trace, since exceptions thrown by a Parser
  * will not have useful stack trace information for end users of the Spac framework.
  *
  * @param spacTrace chain of SpacTraceElements, with the "top" of the stack at the beginning, and the "bottom" of the stack at the end
  * @param detail a `Left` containing a spac-specific error message, or a `Right` containing some non-Spac exception that was caught inside a Parser
  * @tparam Self self-type used in the type signature of `withSpacTrace`
  */
abstract class SpacException[Self <: SpacException[Self]](
	val spacTrace: Chain[SpacTraceElement],
	val detail: Either[String, Throwable],
) extends Exception(
	/* message */ {
		val headLine = detail match {
			case Left(spacMessage) => spacMessage
			case Right(nonSpacCause) => s"Downstream logic error: $nonSpacCause"
		}
		spacTrace.iterator.map(e => s"\n\tat ${ e.render }").mkString(headLine, "", "")
	},
	/* cause */ {
		detail match {
			case Left(spacMessage) => null
			case Right(nonSpacCause) => nonSpacCause
		}
	}
) with NoStackTrace
{
	def this(spacTrace: Chain[SpacTraceElement], spacMessage: String) = this(spacTrace, Left(spacMessage))
	def this(spacTrace: Chain[SpacTraceElement], nonSpacCause: Throwable) = this(spacTrace, Right(nonSpacCause))

	/** Used internally by the framework, typically in a Transformer.Handler's `unwind` method */
	def withSpacTrace(spacTrace2: Chain[SpacTraceElement]): Self
	/** Used internally by the framework, typically in a Transformer.Handler's `unwind` method */
	def addTrace(nextTrace: Chain[SpacTraceElement]): Self = withSpacTrace(spacTrace ++ nextTrace)
	/** Used internally by the framework, typically in a Transformer.Handler's `unwind` method */
	def addTrace(nextTraceElems: SpacTraceElement*): Self = addTrace(Chain(nextTraceElems: _*))
	/** Used internally by the framework, typically in a Transformer.Handler's `unwind` method */
	def addEarlyTrace(firstTrace: Chain[SpacTraceElement]): Self = withSpacTrace(firstTrace ++ spacTrace)
	/** Used internally by the framework, typically in a Transformer.Handler's `unwind` method */
	def addEarlyTrace(firstTraceElems: SpacTraceElement*): Self = addEarlyTrace(Chain(firstTraceElems: _*))
}

object SpacException {
	def addTrace(cause: Throwable, nextTraceElems: Chain[SpacTraceElement]): Throwable = cause match {
		case _ if nextTraceElems.isEmpty => cause
		case se: SpacException[_] => se.addTrace(nextTraceElems)
		case nonSpacCause => new CaughtError(nonSpacCause, nextTraceElems)
	}
	def addTrace(cause: Throwable, nextTraceElems: SpacTraceElement*): Throwable = addTrace(cause, Chain(nextTraceElems: _*))

	def addEarlyTrace(cause: Throwable, firstTrace: Chain[SpacTraceElement]): Throwable = cause match {
		case _ if firstTrace.isEmpty => cause
		case se: SpacException[_] => se.addEarlyTrace(firstTrace)
		case nonSpacCause => new CaughtError(nonSpacCause, firstTrace)
	}
	def addEarlyTrace(cause: Throwable, firstTrace: SpacTraceElement*): Throwable = addEarlyTrace(cause, Chain(firstTrace: _*))

	// ------------------------------------------------------------------------------

	class CaughtError(val nonSpacCause: Throwable, spacTrace: Chain[SpacTraceElement])
		extends SpacException[CaughtError](spacTrace, nonSpacCause)
	{
		def withSpacTrace(spacTrace2: Emit[SpacTraceElement]) = new CaughtError(nonSpacCause, spacTrace2)
	}
	object CaughtError {
		def unapply(e: Throwable) = e match {
			case ce: CaughtError => Some(ce.nonSpacCause)
		}
	}

	// ------------------------------------------------------------------------------

	def missingFirst[Out: TypeName] = new MissingFirstException[Out](Chain.nil)
	class MissingFirstException[Out: TypeName](spacTrace: Chain[SpacTraceElement])
		extends SpacException[MissingFirstException[Out]](spacTrace, s"Parser context ended before the first ${ typeName[Out] } could be found.")
	{
		def withSpacTrace(spacTrace2: Emit[SpacTraceElement]) = new MissingFirstException[Out](spacTrace2)
	}

	// ------------------------------------------------------------------------------

	def unexpectedInput[A](input: A, expectations: List[String]) = new UnexpectedInputException[A](input, expectations, Chain.nil)
	class UnexpectedInputException[A](val input: A, val expectations: List[String], spacTrace: Chain[SpacTraceElement])
		extends SpacException[UnexpectedInputException[A]](spacTrace, s"Unexpected input: expected ${ formatExpectations(expectations) }, but got $input.")
	{
		def withSpacTrace(spacTrace2: Emit[SpacTraceElement]) = new UnexpectedInputException[A](input, expectations, spacTrace2)
	}
	object UnexpectedInputException {
		def unapply(e: Throwable) = e match {
			case uie: UnexpectedInputException[_] => Some(uie.input -> uie.expectations)
			case _ => None
		}
	}

	// ------------------------------------------------------------------------------

	def unfulfilledInputs(expectations: List[String]) = new UnfulfilledInputsException(expectations, Chain.nil)
	class UnfulfilledInputsException(val expectations: List[String], spacTrace: Chain[SpacTraceElement])
		extends SpacException[UnfulfilledInputsException](spacTrace, s"Unexpected end of input: still expected ${ formatExpectations(expectations) }.")
	{
		def withSpacTrace(spacTrace2: Emit[SpacTraceElement]) = new UnfulfilledInputsException(expectations, spacTrace2)
	}
	object UnfulfilledInputsException {
		def unapply(e: Throwable) = e match {
			case uie: UnfulfilledInputsException => Some(uie.expectations)
			case _ => None
		}
	}

	private def formatExpectations(expectations: List[String]) = expectations.tail match {
		case Nil => expectations.head
		case list => (expectations.head :: list).mkString("[", ", then ", "]")
	}

	// ------------------------------------------------------------------------------

	def fallbackChainFailure(underlyingErrors: List[Throwable]) = new FallbackChainFailure(underlyingErrors, Chain.nil)
	class FallbackChainFailure(val underlyingErrors: List[Throwable], spacTrace: Chain[SpacTraceElement])
		extends SpacException[FallbackChainFailure](spacTrace, "Every parser in the fallback chain failed.")
	{
		for (err <- underlyingErrors) this.addSuppressed(err)

		def withSpacTrace(spacTrace2: Emit[SpacTraceElement]) = new FallbackChainFailure(underlyingErrors, spacTrace2)
	}
	object FallbackChainFailure {
		def unapply(e: Throwable) = e match {
			case fcf: FallbackChainFailure => Some(fcf.underlyingErrors)
			case _ => None
		}
	}
}