package io.dylemma.spac

import cats.Applicative
import cats.data.Chain
import fs2.Pipe
import io.dylemma.spac.impl._
import org.tpolecat.typename.TypeName

import scala.annotation.tailrec
import scala.util.Try

/** Primary "spac" abstraction which represents a sink for data events.
  *
  * Parsers are responsible for interpreting a stream of `In` events as a single result of type `Out`.
  * The actual interpretation is performed by a `Parser.Handler` which the Parser is responsible for constructing.
  * Handlers may be internally-mutable, and so they are generally only constructed by the `parse` helper methods or by other handlers.
  * Parsers themselves are immutable, acting as "handler factories", and so they may be freely reused.
  *
  * A parser differs from typical "fold" operations in that it may choose to abort early with a result,
  * leaving the remainder of the data stream untouched.
  *
  * @tparam In  event/input type
  * @tparam Out result type
  * @groupname abstract Abstract Members
  * @groupname combinators Transformation / Combinator Methods
  * @groupname consumers Consumer Methods
  * @groupprio absract 0
  * @groupprio consumers 1
  * @groupprio combinators 2
  */
trait Parser[-In, +Out] { self =>

	/** Parser's main abstract method; constructs a new Handler representing this parser's logic.
	  * Parsers are expected to be immutable, but Handlers may be internally-mutable.
	  *
	  * @group abstract
	  */
	def newHandler: Parser.Handler[In, Out]

	/** Creates a copy of this parser, but with a different `toString`
	  *
	  * @param name The new "name" (i.e. `toString`) for this parser
	  * @return A copy of this parser whose `toString` returns the given `name`
	  * @group combinators
	  */
	def withName(name: String): Parser[In, Out] = new ParserWithName(this, name)

	/** Create a copy of this Parser whose result is transformed by the given function `f`.
	  *
	  * @param f Result transformation function
	  * @tparam Out2 The new parser's result type
	  * @return
	  * @group combinators
	  */
	def map[Out2](f: Out => Out2): Parser[In, Out2] = new ParserMapped(this, f)

	/** Combine this parser with the `fallback` such that failures from the underlying parsers will be ignored as long as at least one succeeds.
	  * The result will be the result of whichever underlying parser succeeds first.
	  * If all of the underlying parsers fail, a `SpacException.FallbackChainFailure` will be thrown by the returned parser's handler.
	  *
	  * @param fallback another parser of the same(ish) type as this one
	  * @tparam In2  Subtype of `In`, or just `In` (to satisfy Parser's contravariance on the `In` type)
	  * @tparam Out2 Supertype of `Out`, or just `Out` (to satisfy Parser's covariance on the `Out` type)
	  * @return A new parser that will succeed if either this parser or the fallback succeed
	  * @group combinators
	  */
	def orElse[In2 <: In, Out2 >: Out](fallback: Parser[In2, Out2]): Parser[In2, Out2] = ParserOrElseChain[In2, Out2](Chain(this, fallback))

	/** Create a copy of this Parser whose handler will catch NonFatal exceptions thrown by the underlying logic.
	  * Caught exceptions will be yielded as a `Failure` output. Normal results will be wrapped in `Success`.
	  *
	  * @return A copy of this parser that will return a `Failure` instead of throwing exceptions
	  * @group combinators
	  */
	def wrapSafe: Parser[In, Try[Out]] = new ParserTry(this)

	/** Creates a copy of this parser which unwraps the resulting `Try`, throwing an exception if the result was a `Failure`.
	  * This operation is the opposite of `wrapSafe`.
	  *
	  * @group combinators
	  */
	def unwrapSafe[T](implicit ev: Out <:< Try[T]): Parser[In, T] = new ParserRethrow(this.upcast[Try[T]])

	/** Like `wrapSafe`, but represents exceptions as `Left` and successful results as `Right`
	  *
	  * @group combinators
	  */
	def attempt: Parser[In, Either[Throwable, Out]] = wrapSafe.map(_.toEither)

	/** Like `unwrapSafe`, but rethrows exceptions from `Left` or returns results from `Right`.
	  * This operation is the opposite of `attempt`.
	  *
	  * @param ev
	  * @tparam T
	  * @return
	  * @group combinators
	  */
	def rethrow[T](implicit ev: Out <:< Either[Throwable, T]): Parser[In, T] = upcast[Either[Throwable, T]].map(_.toTry).unwrapSafe

	/** Impose expectations on the sequence of inputs to be received by handlers created by this parser.
	  * As this parser's handler receives an input, the input will be tested against the head of the expectations list.
	  * If the test returns `false`, the expectation is failed and the handler will throw an exception.
	  * If the test returns `true`, the expectation is satisfied, and the handler will advance to the next expectation.
	  * If there are no more expectations left in the list (i.e. N inputs have satisfied the corresponding N expectations),
	  * then all expectations have been met and inputs will be treated as normal by the handler.
	  * If the handler receives an EOF before all expectations are met, it will throw an exception.
	  *
	  * @param expectations A sequence of `label -> test` expectations imposed on inputs to this parser
	  * @return A copy of this parser with expectations imposed on its inputs
	  * @group combinators
	  */
	def expectInputs[I2 <: In](expectations: List[(String, I2 => Boolean)]): Parser[I2, Out] = new ParserExpectInputs(this, expectations)

	/** Create a copy of this parser that will treat a result from the `interrupter` as an early EOF.
	  * This is especially useful for creating `followedBy` chains involving optional elements.
	  *
	  * Normally, a parser for an optional item in some context will not finish until that context ends,
	  * or until the item is encountered. So if the item is not present, `followedBy` logic won't work
	  * since the `followUp` parser/transformer will not see any events.
	  *
	  * To make sure the leading parser can "fail fast", you can "interrupt" it, typically by creating
	  * a parser that immediately returns a result upon entering a particular context, i.e. the context
	  * in which the "following" parser will start. `Parser#beforeContext` provides a convenience for
	  * doing so.
	  *
	  * Note that if the `interrupter` throws an exception, that exception will not be caught.
	  * If your interrupter might throw, pass `interrupter.wrapSafe` instead to swallow the exception.
	  *
	  * @param interrupter A parser which will be run in parallel with this parser, and whose result will
	  *                    be treated as an early EOF for this parser, forcing an early call to `finish()`.
	  * @tparam I2 Subtype of `In`, or just `In` (to satisfy contravariance of Parser's `In` type)
	  * @return A parser which will perform an early `finish()` call when the `interrupter` produces a result.
	  * @group combinators
	  */
	def interruptedBy[I2 <: In](interrupter: Parser[I2, Any]): Parser[I2, Out] = new ParserInterruptedBy(this, interrupter)

	/** Specialization of `interruptedBy` for stack-like input types, such that an interruption will occur upon entering
	  * a stack context that can be matched by the given `matcher`.
	  *
	  * Example:
	  * {{{
	  * val preludeContext = * \ "prelude"
	  * val dataContext = * \ "data"
	  * for {
	  *   prelude <- Splitter(preludeContext).firstOption[Prelude].beforeContext(dataContext).followedByStream
	  *   data <- Splitter(dataContext).as[Data]
	  * } yield data
	  * }}}
	  *
	  * @param matcher   A matching function that operates on a context stack
	  * @param stackable Interprets the inputs as stack push/pop events to accumulate a context stack
	  * @tparam I2        Subtype of `In`, or just `In` (to satisfy contravariance of Parser's `In` type)
	  * @tparam StackElem Specialization of the `In` type for when it represents a stack push or pop
	  * @return A parser which will perform an early `finish()` when a matching context is encountered
	  * @group combinators
	  */
	def beforeContext[I2 <: In, StackElem](matcher: ContextMatcher[StackElem, Any])(implicit stackable: StackLike[I2, StackElem], pos: CallerPos): Parser[I2, Out] = {
		// use ContextMatchSplitter to drive the stackable+matcher together, and pipe it into a parser that returns when a ContextPush is interpreted,
		// i.e. the `interrupter` will yield a result upon entering a context matched by the `matcher`
		interruptedBy { Splitter[I2].fromMatcher(matcher).addBoundaries.collect { case Left(ContextPush(_, _)) => () } into Parser.firstOpt }
	}

	/** Returns this parser, with the output type widened to `Out2`, which is some supertype of `Out`.
	  * Uses `asInstanceOf` rather than creating a new parser.
	  *
	  * @group combinators
	  */
	def upcast[Out2](implicit ev: Out <:< Out2): Parser[In, Out2] = this.asInstanceOf[Parser[In, Out2]]

	/** Represent this parser as a `Transformer` which emits this parser's result
	  *
	  * @group combinators
	  */
	def asTransformer: Transformer[In, Out] = new ParserAsTransformer(this)

	/** Interpret the given `source` as a data stream of type `In`, using this parser to produce a result of type `Out`.
	  * Exceptions thrown by the underlying parser logic will bubble up and be thrown by this method.
	  *
	  * @param source The source of a data stream
	  * @param S      Typeclass instance that provides the logic for feeding values from `source` into this parser
	  * @param pos    Captures the caller filename and line number, used to fill in the 'spac trace' if the parser throws an exception
	  * @tparam S The source type. Will typically be a `File` or a `List[In]`.
	  * @return
	  * @group consumers
	  */
	@throws[SpacException[_]]
	def parse[S](source: S)(implicit S: Parsable[cats.Id, S, In], pos: CallerPos): Out = {
		S.parse[Out](source, SpacTraceElement.InParse("parser", "parse", pos), this)
	}

	/** Convert this parser to a FS2 "Pipe".
	  * The resulting pipe will forward inputs from the upstream into this parser,
	  * emitting a single value to the downstream when this parser finishes.
	  * Since a `Parser` may abort early (e.g. with `Parser.first`),
	  * the pipe may not pull the entire input stream.
	  *
	  * @param pos
	  * @tparam F
	  * @return
	  */
	def toPipe[F[_]](implicit pos: CallerPos): Pipe[F, In, Out] = ParserToPipe(this, SpacTraceElement.InParse("parser", "toPipe", pos))

}

/** Convenience version of the `Parser` companion object, which provides parser constructors with the `In` type already specified.
  * Integrations for XML and JSON will generally create implicit classes to add methods to this class for `In = XmlEvent` and `In = JsonEvent` respectively.
  *
  * @tparam In
  */
class ParserApplyWithBoundInput[In] {
	def apply[Out](implicit parser: Parser[In, Out]): Parser[In, Out] = parser
	def firstOpt: Parser[In, Option[In]] = Parser.firstOpt
	def first(implicit In: TypeName[In]): Parser[In, In] = Parser.first
	def find(predicate: In => Boolean): Parser[In, Option[In]] = Parser.find(predicate)
	def fold[Out](init: Out)(op: (Out, In) => Out): Parser[In, Out] = Parser.fold(init)(op)
	def pure[Out](value: Out): Parser[In, Out] = Parser.pure(value)
	def delay[Out](value: => Out): Parser[In, Out] = Parser.delay(value)
	def defer[Out](p: => Parser[In, Out]): Parser[In, Out] = Parser.defer(p)
	def deferHandler[Out](h: => Parser.Handler[In, Out]): Parser[In, Out] = Parser.deferHandler(h)
	def fromBuilder[Out](b: => collection.mutable.Builder[In, Out]): Parser[In, Out] = Parser.fromBuilder(b)
	def toList: Parser[In, List[In]] = Parser.toList
	def toChain: Parser[In, Chain[In]] = Parser.toChain
	def toMap[K, V](implicit ev: In <:< (K, V)): Parser[(K, V), Map[K, V]] = Parser.toMap[K, V]
	def tap(f: In => Unit): Parser[In, Unit] = Parser.tap(f)
	def drain: Parser[In, Unit] = Parser.drain
	def oneOf[Out](parsers: Parser[In, Out]*): Parser[In, Out] = Parser.oneOf(parsers: _*)
}

object Parser {
	/** Specialization for Parsers which require no mutable state.
	  * A "stateless" parser acts as its own handler.
	  */
	trait Stateless[-In, +Out] extends Parser[In, Out] with Handler[In, Out] {
		def newHandler: this.type = this
	}

	/** An internally-mutable representation of a Parser, which reacts to inputs from a data stream and eventually produces a result.
	  */
	trait Handler[-In, +Out] {
		/** Advance the state of this handler by accepting a single input of type `In`.
		  * If doing so would cause this parser to complete, return a `Left` containing the output.
		  * Otherwise, return a `Right` containing the next parser state.
		  *
		  * Handlers are assumed to be internally-mutable, so it is acceptable to simply
		  * update some internal state and then return `Right(this)`, although in some
		  * cases it will be desirable to return a separate handler entirely.
		  *
		  * @param in A single input event from a data stream
		  * @return If the input would finish the parser, return a `Left` containing the result.
		  *         Otherwise, return a `Right` containing a Handler which represents the next parsing state.
		  *         The handler in a `Right` may be this handler, or a completely separate one.
		  */
		def step(in: In): Either[Out, Handler[In, Out]]

		/** Signal the end of the data stream to this handler, forcing it to generate a result.
		  * Handlers may throw exceptions in response to this, such as a handler which wants the first event from an empty stream.
		  *
		  * Further calls to `step` or `finish` after the first call to `finish` will result in undefined behavior.
		  * The general assumption is that a handler should be discarded after its `finish` method is called.
		  *
		  * @return the final result of this parser
		  */
		def finish(): Out

		/** Convenience function to call `step` on a sequence of inputs all at once.
		  * If the `step` returns a result, this method will return a `Left` containing that result and the remainder of the `inputs` that were not consumed.
		  * If the `inputs` run out before the handler returns a result from a `step`, this method will return a `Right` containing the latest state of the handler.
		  * This method will not call the handler's `finish()`.
		  *
		  * In general, you won't call this method directly. Instead, use one of the Parser trait's `parse` methods.
		  *
		  * @param inputs A sequence of inputs
		  * @param C      Evidence that the `inputs` has a `head/tail` split operation
		  * @tparam C   An `Unconsable` collection, i.e. `List` or `cats.data.Chain`
		  * @tparam In2 Subtype of `In`, or `In` (to satisfy contravariance)
		  * @return Either the handler's result paired with the remaining inputs, or the new handler state
		  */
		def stepMany[C[_], In2 <: In](inputs: C[In2])(implicit C: Unconsable[C]): Either[(Out, C[In2]), Handler[In, Out]] = {
			@tailrec def loop(current: Handler[In, Out], remaining: C[In2]): Either[(Out, C[In2]), Handler[In, Out]] = {
				C.uncons(remaining) match {
					case Some((head, tail)) =>
						current.step(head) match {
							case Right(cont) => loop(cont, tail)
							case Left(out) => Left(out -> tail)
						}
					case None =>
						Right(current)
				}
			}

			loop(this, inputs)
		}

		/** Wraps this handler as a "top level" handler, which will inject a SpacTraceElement
		  * (representing the current input or the "EOF" signal)
		  * to any exception is thrown by this handler when calling its `step` or `finish` methods.
		  *
		  * Used internally by `Parser`'s `parse` methods.
		  */
		def asTopLevelHandler(caller: SpacTraceElement): Handler[In, Out] = new TopLevelParserHandler(this, caller)
	}

	implicit final class InvariantOps[In, Out](private val self: Parser[In, Out]) extends AnyVal {
		/** Interpret the given `source` as a data stream of type `In`, using this parser to produce a result of type `Out`.
		  * In this version of the `parse` method, the data-pull and handler logic is run in the `F` context.
		  * Exceptions thrown by the underlying parser logic will be raised in the F context instead of thrown.
		  *
		  * @param source The source of a data stream
		  * @param S      Typeclass instance that provides the logic for feeding values from `source` into this parser
		  * @param pos    Captures the caller filename and line number, used to fill in the 'spac trace' if the parser throws an exception
		  * @tparam F An effect type in which the data-pull and handler logic will be run
		  * @tparam S The source type. Will typically be a `File` or a `List[In]`.
		  * @return An effect which, when evaluated, will consume data events from the `source` using this parser to produce a result.
		  *         Note that if the `F` type isn't "lazy", the actual evaluation of the stream may happen as part of the *construction*
		  *         of the `F[Out]`, rather than during the *evaluation*.
		  * @group consumers
		  */
		def parseF[F[_], S](source: S)(implicit S: Parsable[F, S, In], pos: CallerPos): F[Out] = {
			S.parse[Out](source, SpacTraceElement.InParse("parser", "parseF", pos), self)
		}
	}

	/** Convenience for creating parsers whose input type is bound to `In`.
	  *
	  * This is particularly nice when the `Out` type can be inferred by the compiler, e.g.
	  * {{{
	  * Parser[Int].fold(1)(_ + _)
	  * // versus
	  * Parser.fold[Int, Int](1)(_ + _)
	  * }}}
	  */
	def apply[In] = new ParserApplyWithBoundInput[In]

	/** Creates a parser which returns the first input it receives, or None when handling an empty stream.
	  */
	def firstOpt[In]: Parser[In, Option[In]] = new ParserFirstOpt[In]

	@deprecated("Use `firstOpt` instead", "v0.9")
	def firstOption[In]: Parser[In, Option[In]] = firstOpt[In]

	/** Creates a parser which returns the first input it receives, or throws a `SpacException.MissingFirstException` when handling an empty stream.
	  */
	def first[In: TypeName]: Parser[In, In] = new ParserFirst[In]

	/** Creates a parser which returns the first input for which the `predicate` function returns true.
	  */
	def find[In](predicate: In => Boolean): Parser[In, Option[In]] = new ParserFind(predicate)

	/** Creates a parser which folds inputs into its state according to the `op` function, returning its final state when the input stream ends.
	  *
	  * @param init The initial state
	  * @param op   state update function
	  */
	def fold[In, Out](init: Out)(op: (Out, In) => Out): Parser[In, Out] = new ParserFold(init, op)

	/** Creates a parser which always results in `value`.
	  *
	  * @param value the result of the parser
	  */
	def pure[Out](value: Out): Parser[Any, Out] = new ParserPure(value)

	/** Creates a parser which evaluates the call-by-name `value` expression.
	  * The `value` expression won't be evaluated until a handler created by this parser is asked to `step` or `finish`.
	  * The value is not memoized, so if a new handler is created, that handler will re-evaluate the value.
	  *
	  * @param value A call-by-name expression which will be evaluated by the returned parser's `Handler`
	  * @tparam Out the result type
	  */
	def delay[Out](value: => Out): Parser[Any, Out] = new ParserDelay(() => value)

	/** Creates a parser which delegates to the given call-by-name `p` parser.
	  * The `p` expression isn't evaluated until a handler created by this parser is created - that handler will be `p`'s handler.
	  * The underlying parser is not memoized, so if a new handler is constructed, the `p` expression will be re-evaluated.
	  *
	  * @param p A call-by-name expression which returns a Parser
	  * @tparam In  The parser's input type
	  * @tparam Out The parser's output type
	  */
	def defer[In, Out](p: => Parser[In, Out]): Parser[In, Out] = new ParserDefer(() => p)

	/** Creates a parser from a call-by-name handler construction expression.
	  * This is effectively doing
	  * {{{
	  * new Parser[In, Out] {
	  *   def newHandler = h
	  * }
	  * }}}
	  */
	def deferHandler[In, Out](h: => Parser.Handler[In, Out]): Parser[In, Out] = new ParserDeferHandler(() => h)

	/** Creates a parser whose handlers which will construct a new builder via the call-by-name `b` expression.
	  * The builder's result will be used as the result of the handler.
	  */
	def fromBuilder[In, Out](b: => collection.mutable.Builder[In, Out]): Parser[In, Out] = deferHandler { new ParserHandlerForBuilder(b) }

	/** Creates a parser that builds a List from the inputs it receives.
	  */
	def toList[In]: Parser[In, List[In]] = fromBuilder { List.newBuilder }

	/** Creates a parser that builds a `cats.data.Chain` from the inputs it receives.
	  */
	def toChain[In]: Parser[In, Chain[In]] = fold(Chain.empty[In])(_ :+ _)

	/** Creates a parser that builds a Map from the `(key, value)` tuple inputs it receives.
	  */
	def toMap[K, V]: Parser[(K, V), Map[K, V]] = fromBuilder { Map.newBuilder[K, V] }

	/** Create a parser which runs the side-effecting function `f` for each input it receives, yielding a Unit result.
	  */
	def tap[In](f: In => Unit): Parser[In, Unit] = new ParserTap(f)

	@deprecated("Use `tap` instead", "v0.9")
	def foreach[In](f: In => Any): Parser[In, Unit] = tap(in => f(in))

	/** Create a parser which will consume the entire input stream, ignoring each value, yielding a Unit result when the stream ends.
	  */
	def drain: Parser[Any, Unit] = ParserDrain

	@deprecated("use `.pure` instead", "v0.9")
	def constant[Out](value: Out) = pure(value)

	/** Create a single parser which will attempt to run each of the given `parsers` in parallel,
	  * yielding the result from the first of the `parsers` that successfully completes.
	  * If multiple `parsers` return a result for the same input, priority is determined by
	  * their order when given to this method.
	  * If all of the `parsers` fail by throwing exceptions, all but the latest exception
	  * will be swallowed, and the last exception will be rethrown.
	  *
	  * @param parsers A collection of Parsers which will be run in parallel.
	  * @tparam In
	  * @tparam Out
	  */
	def oneOf[In, Out](parsers: Parser[In, Out]*): Parser[In, Out] = ParserOrElseChain(Chain.fromSeq(parsers))

	/** Adds `followedBy` and `followedByStream` to Parser (they aren't defined in the Parser trait due to the `In` type needing to be invariant here) */
	implicit class ParserFollowedByOps[In, A](parser: Parser[In, A]) {
		/** Alias for `followedBy`, for use when Cat's `ApplyOps` gets in the way with its own useless `followedBy` method.
		  *
		  * @return
		  * @group combinators
		  */
		def followedByParser = followedBy

		/** Intermediate object for creating a sequenced parser in which the result of this parser will
		  * be used to initialize a second parser as soon as it is available.
		  *
		  * In other words, the source (series of `In` values) will be fed into this Parser until this
		  * parser's handler returns a result of type `Out`. At that point, the second parser (as
		  * specified by using the `apply` or `flatMap` methods on the `FollowedBy` returned by this method)
		  * will be instantiated. Any relevant "stack events" (see `Stackable`) will be replayed so the
		  * second parser has the right context, and from that point on, all `In` values will be sent
		  * to the second parser. When that second parser returns a result, that result becomes the output
		  * of the combined parser created by `this.followedBy(out => makeSecondParser(out))`
		  *
		  * Examples:
		  * {{{
		  *    val p1: Parser[A] = /* ... */
		  *    def getP2(p1Result: A): Parser[B] = /* ... */
		  *    val combined: Parser[B] = p1.followedBy(getP2)
		  *
		  *    // alternative `flatMap` syntax
		  *    val combined: Parser[B] = for {
		  *      p1Result <- p1.followedBy
		  *      p2Result <- getP2(p1Result)
		  *    } yield p2Result
		  * }}}
		  *
		  * See Parser's `interruptedBy`, which is useful when a `transformer.parseFirstOption`
		  * must be `followedBy` some other parser.
		  *
		  * @group combinators
		  */
		def followedBy: FollowedBy[In, A, Parser] = new FollowedBy[In, A, Parser] {
			def apply[Out](followUp: A => Parser[In, Out])(implicit S: StackLike[In, Any]): Parser[In, Out] = {
				new ParserFollowedByParser(parser, followUp)
			}
		}

		/** Intermediate object creating a transformer that depends on this parser.
		  * Particularly useful in cases where one or more specific "info" elements precede
		  * a stream of other elements which require that "info" to be parsed.
		  *
		  * Examples:
		  * {{{
		  *    val p1: Parser[In, A] = /* ... */
		  *    def getP2Stream(p1Result: A): Transformer[In, B] = /* ... */
		  *    val combined: Transformer[In, B] = p1.andThenStream(getP2Stream)
		  *
		  *    // alternative `flatMap` syntax
		  *    val combined: Transformer[In, B] = for {
		  *      p1Result <- p1.andThenStream
		  *      p2Result <- getP2Stream(p1Result)
		  *    } yield p2Result
		  * }}}
		  *
		  * See `followedBy` for a general explanation of how the combination works.
		  *
		  * See also, `interruptedBy`, which is useful when a `transformer.parseFirstOption`
		  * must be `followedBy` some other transformer.
		  *
		  * @group combinators
		  */
		def followedByStream: FollowedBy[In, A, Transformer] = new FollowedBy[In, A, Transformer] {
			def apply[Out](followUp: A => Transformer[In, Out])(implicit S: StackLike[In, Any]): Transformer[In, Out] = {
				new ParserFollowedByTransformer(parser, followUp)
			}
		}
	}

	/** Applicative for Parser with a fixed `In` type. */
	implicit def catsApplicativeForParser[In](implicit callerPos: CallerPos): Applicative[Parser[In, *]] = new Applicative[Parser[In, *]] {
		def pure[A](x: A) = new ParserPure(x)
		def ap[A, B](ff: Parser[In, A => B])(fa: Parser[In, A]) = product(fa, ff).map { case (a, f) => f(a) }
		override def product[A, B](fa: Parser[In, A], fb: Parser[In, B]) = {
			(fa, fb) match {
				case (faCompound: ParserCompoundN[In, A], fbCompound: ParserCompoundN[In, B]) =>
					val offset = faCompound.members.size.toInt
					new ParserCompoundN(
						faCompound.members ++ fbCompound.members,
						get => faCompound.assemble(get) -> fbCompound.assemble(OffsetGet(offset, get)),
						callerPos,
					)
				case (fa, fbCompound: ParserCompoundN[In, B]) =>
					new ParserCompoundN(
						fa +: fbCompound.members,
						get => get(0).asInstanceOf[A] -> fbCompound.assemble(OffsetGet(1, get)),
						callerPos,
					)
				case (faCompound: ParserCompoundN[In, A], fb) =>
					val offset = faCompound.members.size.toInt
					new ParserCompoundN(
						faCompound.members :+ fb,
						get => faCompound.assemble(get) -> get(offset).asInstanceOf[B],
						callerPos,
					)
				case (fa, fb) =>
					new ParserCompoundN(
						Chain(fa, fb),
						results => (results(0).asInstanceOf[A], results(1).asInstanceOf[B]),
						callerPos,
					)
			}
		}
		override def map[A, B](fa: Parser[In, A])(f: A => B): Parser[In, B] = fa.map(f)

		private case class OffsetGet private(offset: Int, get: Int => Any) extends (Int => Any) {
			def apply(i: Int) = get(i + offset)
		}
		private object OffsetGet {
			def apply(offset: Int, get: Int => Any) = get match {
				case OffsetGet(n, g) => new OffsetGet(n + offset, g)
				case g => new OffsetGet(offset, g)
			}
		}

	}

	/** An intermediate object for creating sequence-based combination methods for a Parser or Consumer.
	  *
	  * @tparam M Type constructor for the parser/consumer of a given output type
	  * @tparam A Output type for the "first" parser/consumer; using the combination methods in this trait
	  *           will result in an instance of T1 being used to create a "second" parser/consumer/transformer
	  *           to be run sequentially after the first.
	  */
	trait FollowedBy[In, +A, M[-_, +_]] { self =>
		/** Creates a sequence handler by combining this one and a `getNext` function such that when this
		  * handler finishes, a second handler is created by passing its result ot `getNext`.
		  *
		  * @param followUp A function that creates the new Parser/Transformer (whichever `M` is in this context) based on the result of this parser
		  * @return The combined parser/transformer
		  */
		def apply[Out](followUp: A => M[In, Out])(implicit S: StackLike[In, Any]): M[In, Out]

		/** Alias for `apply`, allowing the use of for-comprehension syntax, e.g.
		  * {{{
		  * for {
		  *   a <- someParser
		  *   result <- getNextParser(a)
		  * } yield result
		  * }}}
		  */
		def flatMap[Out](followUp: A => M[In, Out])(implicit S: StackLike[In, Any]): M[In, Out] = apply(followUp)

		/** Transforms the intermediate result that will be passed to the `followUp` function.
		  * Really this method is only here to allow `for-comprehension` syntax to be used - see `flatMap`.
		  * You probably don't want to call this explicitly.
		  */
		def map[B](f: A => B)(implicit S: StackLike[In, Any]): FollowedBy[In, B, M] = new FollowedBy[In, B, M] {
			def apply[Out](followUp: B => M[In, Out])(implicit S: StackLike[In, Any]) = self { a => followUp(f(a)) }
		}
	}
}