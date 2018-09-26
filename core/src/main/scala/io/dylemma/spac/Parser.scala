package io.dylemma.spac

import io.dylemma.spac.handlers._
import io.dylemma.spac.types.Stackable

import scala.language.higherKinds
import scala.util.Try

/** An immutable object that can be used to create `Handler`s.
	*/
trait Parser[-In, +Out] extends HandlerFactory[In, Out] { self =>

	/** Parse the given source object by spawning a handler and feeding events
	  * from the source into that handler until it yields a result.
	  *
	  * @param source An object that can be treated as a stream of $Event
	  * @param consume Typeclass instance allowing instances of `Src` to be treated as a stream of $Event
	  * @tparam S The source object type
	  * @return The parse result
	  */
	def parse[S](source: S)(implicit consume: ConsumableLike[S, In]): Out = {
		consume(source, makeHandler())
	}

	/** Transform this Parser's results using a transformation function.
	  *
	  * @param f The transformation function
	  * @tparam B The result type of the transformation function
	  * @return A new Parser instance which computes its results by applying `f` to the
	  *         results computed by this Parser
	  */
	def map[U](f: Out => U): Parser[In, U] = new Parser[In, U] {
		def makeHandler(): Handler[In, U] = new MappedConsumerHandler(f, self.makeHandler())
	}

	/** Combine this parser with the `fallback` such that if either one fails but the other succeeds,
	  * the result will be taken from the one that succeeds.
	  *
	  * Note: if you want to have a chain of fallbacks, it will be more efficient to do
	  * `Parser.oneOf(p1, p2, p3, ...)` than doing `p1 orElse p2 orElse p3 orElse ...`.
	  *
	  * @param fallback
	  * @tparam B
	  * @return
	  */
	def orElse[I2 <: In, B >: Out](fallback: Parser[I2, B]): Parser[I2, B] = new Parser[I2, B] {
		override def toString = s"$self.orElse($fallback)"
		def makeHandler(): Handler[I2, B] = new FallbackSetHandler[I2, B](self.makeHandler(), fallback.makeHandler())
	}

	/** Create a new parser to wrap this one, such that any exception thrown during parsing will be caught
	  * and returned as a `Failure`.
	  * @return A parser that will return a Failure instead of throwing an exception
	  */
	def wrapSafe: Parser[In, Try[Out]] = Parser.wrapSafe(this)

	/** Given a parser whose output type is a `Try[T]`, return a new parser which will unwrap the `Try`,
	  * returning either the underlying `T` for `Success`, or throwing the caught exception for `Failure`.
	  * This operation is the opposite of `wrapSafe`.
	  *
	  * @param ev Evidence that this parser's output type is a `Try[T]` for some type `T`
	  * @tparam T The unwrapped type
	  * @return A parser that unwraps the `Try` returned by this parser
	  */
	def unwrapSafe[T](implicit ev: Out <:< Try[T]): Parser[In, T] = Parser.unwrapSafe(asInstanceOf[Parser[In, Try[T]]])

	/** Combine this Parser with another one, so that all inputs will be sent to both `this` and the `other` parser,
	  * and the results of the two parsers will be combined.
	  * Note that the value returned by this method is an intermediate object which should be finalized
	  * by calling its `asTuple` or `as{(a,b) => result}` method, which defines how the results of the two parsers are combined.
	  * Further combinations can be added by calling `and` again on the intermediate object.
	  *
	  * Example:
	  * {{{
	  * val p1: Parser[A] = /* ... */
	  * val p2: Parser[B] = /* ... */
	  * val pc: Parser[(A, B)] = (p1 and p2).asTuple
	  * // or
	  * val pc: Parser[R] = (p1 and p2).as{ (a, b) => combineResults(a, b) }
	  * }}}
	  *
	  * @param other Another Parser to combine with
	  * @tparam O2 The output type of the other Parser
	  * @return An intermediate oject with `as` and `asTuple` methods that finish the combination
	  * @usecase def and[O2](other: Parser[In, O2]): ParserCombination[In]#Combined2[Out, O2]
	  */
	def and[I2 <: In, O2](other: Parser[I2, O2]): ParserCombination[I2]#Combined2[Out, O2] = new ParserCombination[I2]().combine(self, other)

	/** Operator version of `and`
	  *
	  * @param other
	  * @tparam I2
	  * @tparam O2
	  * @return
	  * @usecase def ~[O2](other: Parser[In, O2]): ParserCombination[In]#Combined2[Out, O2]
	  */
	def ~[I2 <: In, O2](other: Parser[I2, O2]): ParserCombination[I2]#Combined2[Out, O2] = new ParserCombination[I2]().combine(self, other)

	/** Creates a sequence handler by combining this one and a `getNext` function such that when this
	  * handler finishes, a second handler is created by passing its result ot `getNext`.
	  *
	  * @param getNext A function that takes this handler's result to create a second handler
	  * @tparam T2 The output type of the second handler
	  * @return The combined handler
	  */

	/** Intermediate object for creating a sequence parser in which the result of this parser will
	  * be used to initialize a second parser as soon as it is available.
	  *
	  * In other words, the source (series of `In` values) will be fed into this Parser until this
	  * parser's handler returns a result of type `Out`. At that point, the second parser (as
	  * specified by using the `apply` or `flatMap` methods on the `FollowedBy` returned by this method)
	  * will be instantiated. Any relevent "stack events" (see `Stackable`) will be replayed so the
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
	  * See `interruptedBy`, which is useful when a `transformer.parseFirstOption`
	  * must be `followedBy` some other parser.
	  */
	def followedBy: FollowedBy[In, Out, Parser] = new FollowedBy[In, Out, Parser] {
		def apply[I2 <: In, T2](getNext: Out => Parser[I2, T2])(implicit stackable: Stackable[I2]): Parser[I2, T2] = new Parser[I2, T2] {
			override def toString = s"$self.followedBy($getNext)"
			def makeHandler(): Handler[I2, T2] = {
				new SequencedInStackHandler[I2, Out, T2](self.makeHandler(), r1 => getNext(r1).makeHandler())
			}
		}
	}

	/** Intermediate object creating a transformer that depends on this parser.
	  * Particularly useful in cases where one or more specific "info" elements precede
	  * a stream of other elements which require that "info" to be parsed.
	  *
	  * Examples:
	  * {{{
	  *    val p1: Parser[A] = /* ... */
	  *    def getP2Stream(p1Result: A): Transformer[$Event, B] = /* ... */
	  *    val combined: Transformer[$Event, B] = p1.andThenStream(getP2Stream)
	  *
	  *    // alternative `flatMap` syntax
	  *    val combined: Transformer[$Event, B] = for {
	  *      p1Result <- p1.andThenStream
	  *      p2Result <- getP2Stream(p1Result)
	  *    } yield p2Result
	  * }}}
	  *
	  * See `followedBy` for a general explanation of how the combination works.
	  *
	  * See also, `interruptedBy`, which is useful when a `transformer.parseFirstOption`
	  * must be `followedBy` some other transformer.
	  */
	def followedByStream: FollowedBy[In, Out, Transformer] = new FollowedBy[In, Out, Transformer] {
		/** Creates a sequence handler by combining this one and a `getNext` function such that when this
		  * handler finishes, a second handler is created by passing its result ot `getNext`.
		  *
		  * @param getNext A function that takes this handler's result to create a second handler
		  * @tparam T2 The output type of the second handler
		  * @return The combined handler
		  */
		def apply[I2 <: In, T2](getTransformer: Out => Transformer[I2, T2])(implicit stackable: Stackable[I2]): Transformer[I2, T2] = new Transformer[I2, T2] {
			override def toString = s"$self.followedByStream($getTransformer)"
			def makeHandler[End](next: Handler[T2, End]): Handler[I2, End] = {
				val handler1 = self.makeHandler()
				def getHandler2(h1Result: Out): Handler[I2, End] = getTransformer(h1Result).makeHandler(next)
				new SequencedInStackHandler(handler1, getHandler2)
			}
		}
	}

	/** Create a copy of this parser that will treat a result from the `interrupter` as an early EOF.
	  * This is especially useful for creating `followedBy` chains involving optional elements.
	  *
	  * Normally, a parser for an optional item in some context will not finish until that context ends,
	  * or until the item is encountered. So if the item is not present, `followedBy` logic won't work
	  * since the following parser/transformer will not see any events.
	  *
	  * To make sure the leading parser can "fail fast", you can "interrupt" it, typically by creating
	  * a parser that immediately returns a result upon entering a particular context, i.e. the context
	  * in which the "following" parser will start. `Parser#beforeContext` provides a convenience for
	  * doing so.
	  *
	  * @param interrupter
	  * @return
	  */
	def interruptedBy[I2 <: In](interrupter: Parser[I2, Any]): Parser[I2, Out] = new Parser[I2, Out] {
		def makeHandler() = new InterrupterHandler[I2, Out](self.makeHandler(), interrupter.makeHandler())
		override def toString = s"$self.interruptedBy($interrupter)"
	}

	/** Create a copy of this parser that will observe an early EOF upon entering a context matched by the
	  * given `matcher`. This is especially useful for creating `followedBy` chains involving optional elements.
	  *
	  * Normally, a parser for an optional item in some context will not finish until that context ends,
	  * or until the item is encountered. So if the item is not present, `followedBy` logic won't work
	  * since the following parser/transformer will not see any events.
	  *
	  * To make sure the leading parser can "fail fast", you can force it to end early if it encounters
	  * a specific context, i.e. the context used by the parser/transformer being passed to `.follwedBy`.
	  *
	  * Example:
	  * {{{
	  * val preludeContext = * \ "prelude"
	  * val dataContext = * \ "data"
	  *
	  * for {
	  *   prelude <- Splitter(preludeContext).firstOption[Prelude].beforeContext(dataContext).followedByStream
	  *   data <- Splitter(dataContext).as[Data]
	  * } yield data
	  * }}}
	  *
	  * @param matcher
	  * @return
	  */
	def beforeContext[I2 <: In, StackElem](matcher: ContextMatcher[StackElem, Any])(implicit stackable: Stackable.Aux[I2, StackElem]): Parser[I2, Out] = {
		val interrupter = new ContextStackSplitter[I2, StackElem, Any](matcher).map(Parser.constant(true)).parseFirst
		self.interruptedBy(interrupter)
	}

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
	  */
	def expectInputs[I2 <: In](expectations: List[(String, I2 => Boolean)]): Parser[I2, Out] = new Parser[I2, Out] {
		override def toString = s"$self.expectInputs($expectations)"
		def makeHandler() = new ExpectationSequenceHandler(expectations, self.makeHandler())
	}

	/** Creates a copy of this parser, but with a different `toString`
	  *
	  * @param name The new "name" (i.e. `toString`) for this parser
	  * @return A copy of this parser whose `toString` returns the given `name`
	  */
	def withName(name: String): Parser[In, Out] = new Parser[In, Out] {
		override def toString = name
		def makeHandler(): Handler[In, Out] = self.makeHandler()
	}
}

object Parser {

	def wrapSafe[In, Out](self: Parser[In, Out]): Parser[In, Try[Out]] = new Parser[In, Try[Out]] {
		def makeHandler(): Handler[In, Try[Out]] = new SafeConsumerHandler(self.makeHandler())
	}

	def unwrapSafe[In, Out](self: Parser[In, Try[Out]]): Parser[In, Out] = new Parser[In, Out] {
		def makeHandler(): Handler[In, Out] = new UnwrapSafeConsumerHandler(self.makeHandler())
	}

	def toList[A]: Parser[A, List[A]] = new Parser[A, List[A]] {
		def makeHandler(): Handler[A, List[A]] = {
			new ToListHandler[A]
		}
		override def toString = "ToList"
	}

	def first[A]: Parser[A, A] = new Parser[A, A] {
		def makeHandler(): Handler[A, A] = {
			new GetFirstHandler[A]
		}
		override def toString = "First"
	}

	def firstOption[A]: Parser[A, Option[A]] = new Parser[A, Option[A]] {
		def makeHandler(): Handler[A, Option[A]] = {
			new GetFirstOptionHandler[A]
		}
		override def toString = "FirstOption"
	}

	def fold[A, R](init: R, f: (R, A) => R): Parser[A, R] = new Parser[A, R] {
		def makeHandler(): Handler[A, R] = {
			new FoldHandler(init, f)
		}
		override def toString = s"Fold($init, $f)"
	}

	def foreach[A](f: A => Any): Parser[A, Unit] = new Parser[A, Unit] {
		def makeHandler() = new ForEachHandler(f)
		override def toString = s"ForEach($f)"
	}

	def constant[A](result: A): Parser[Any, A] = new Parser[Any, A] {
		def makeHandler(): Handler[Any, A] = new ConstantHandler(result)
		override def toString = s"Constant($result)"
	}

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
	  * @return
	  */
	def oneOf[In, Out](parsers: Parser[In, Out]*): Parser[In, Out] = new Parser[In, Out] {
		def makeHandler() = new FallbackSetHandler(parsers.map(_.makeHandler()): _*)
	}
}