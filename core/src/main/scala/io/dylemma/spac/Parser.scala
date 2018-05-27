package io.dylemma.spac

import io.dylemma.spac.handlers._
import io.dylemma.spac.types.Stackable

import scala.language.higherKinds

/** Base class for parsers dealing with a fixed input type, e.g. XMLEvent or JSONEvent.
  *
  * @param handlerFactoryConverter
  * @param stackable
  * @tparam In The generated handler's input type
  * @tparam Out The generated handler's output type
  * @tparam Self
  *
  * @define Event event
  * @define pl parser
  * @define PL Parser
  */
abstract class ParserLike[In, StackElem, +Out, Self[+o] <: HandlerFactory[In, o]](
	protected implicit val handlerFactoryConverter: FromHandlerFactory[In, Self],
	protected implicit val stackable: Stackable.Aux[In, StackElem]
) extends HandlerFactory[In, Out] { self =>

	/** Parse the given source object by spawning a handler and feeding events
	  * from the source into that handler until it yields a result.
	  *
	  * @param src An object that can be treated as a stream of $Event
	  * @param cl Typeclass instance allowing instances of `Src` to be treated as a stream of $Event
	  * @tparam Src The source object type
	  * @return The parse result
	  */
	def parse[Src](src: Src)(implicit cl: ConsumableLike[Src, In]): Out = {
		cl(src, makeHandler())
	}

	/** Convert this $PL to a Consumer.
	  *
	  * @return A Consumer with the same behavior as this $PL
	  */
	def toConsumer: Consumer[In, Out] = new Consumer[In, Out] {
		def makeHandler() = self.makeHandler()
		override def toString = self.toString
	}

	/** Convert this $PL to another handler factory type.
	  *
	  * @param sfhf Typeclass instance facilitating construction of an `S[Out]` from
	  *             the HandlerFactory logic in this $PL.
	  * @tparam S Another handler factory type
	  * @return An instance of `S[Out]` with the same behavior as this $PL
	  */
	def to[S[+_]](implicit sfhf: FromHandlerFactory[In, S]): S[Out] = {
		sfhf.makeInstance(this, this.toString)
	}

	/** Starting point for $PL combination methods.
	  *
	  * @return A HandlerCombination instance for $PL
	  */
	@inline protected def combination = new HandlerCombination[In, Self]

	/** Combine this $PL with another one.
	  * Note that the value returned by this method is an intermediate object which should be finalized
	  * by calling its `asTuple` or `as{(a,b) => result}` method.
	  * Further combinations can be added by calling `and` again on the intermediate object.
	  *
	  * Example:
	  * {{{
	  * val p1: $PL[A] = /* ... */
	  * val p2: $PL[B] = /* ... */
	  * val pc: $PL[(A, B)] = (p1 and p2).asTuple
	  * // or
	  * val pc: $PL[R] = (p1 and p2).as{ (a, b) => combineResults(a, b) }
	  * }}}
	  *
	  * @param other Another $PL to combine with
	  * @tparam O2 The output type of the other $PL
	  * @return An intermediate oject with `as` and `asTuple` methods that finish the combination
	  */
	def and[O2](other: Self[O2]): HandlerCombination[In, Self]#Combined2[Out, O2] = combination.combine(this, other)

	/** Operator version of `and`; combines this $PL with another one.
	  * Note that the value returned by this method is an intermediate object which should be finalized
	  * by calling its `asTuple` or `as{(a,b) => result}` method.
	  * Further combinations can be added by calling `~` again on the intermediate object.
	  *
	  * Example:
	  * {{{
	  * val p1: $PL[A] = /* ... */
	  * val p2: $PL[B] = /* ... */
	  * val pc: $PL[(A, B)] = (p1 and p2).asTuple
	  * // or
	  * val pc: $PL[R] = (p1 and p2).as{ (a, b) => combineResults(a, b) }
	  * }}}
	  *
	  * @param other Another $PL to combine with
	  * @tparam O2 The output type of the other $PL
	  * @return An intermediate oject with `as` and `asTuple` methods that finish the combination
	  */
	def ~[O2](other: Self[O2]): HandlerCombination[In, Self]#Combined2[Out, O2] = combination.combine(this, other)

	/** Transform this $PL's results using a transformation function.
	  *
 	  * @param f The transformation function
	  * @tparam B The result type of the transformation function
	  * @return A new $PL instance which computes its results by applying `f` to the
	  *         results computed by this $PL
	  */
	def map[B](f: Out => B): Self[B] = handlerFactoryConverter.makeInstance(
		new HandlerFactory[In, B] {
			def makeHandler() = new MappedConsumerHandler(f, self.makeHandler())
		},
		s"$self.map($f)"
	)

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
	def orElse[B >: Out](fallback: Self[B]): Self[B] = handlerFactoryConverter.makeInstance(
		new HandlerFactory[In, B] {
			def makeHandler() = new FallbackSetHandler[In, B](self.makeHandler(), fallback.makeHandler())
		},
		s"$self.orElse($fallback)"
	)

	/** An intermediate object with an `apply` and `flatMap` that both create a sequenced $PL
	  * which combines this $PL with a function to create the next one.
	  *
	  * Examples:
	  * {{{
	  *    val p1: $PL[A] = /* ... */
	  *    def getP2(p1Result: A): $PL[B] = /* ... */
	  *    val combined: $PL[B] = p1.followedBy(getP2)
	  *
	  *    // alternative `flatMap` syntax
	  *    val combined: $PL[B] = for {
	  *      p1Result <- p1.followedBy
	  *      p2Result <- getP2(p1Result)
	  *    } yield p2Result
	  * }}}
	  *
	  * An example of where this is useful is when a $pl for XML element depends on values
	  * parsed from one of its previous siblings, but where you don't want to wait until the
	  * end of their parent element before they can be combined.
	  *
	  * @return An intermediate object which has an `apply` and `flatMap` that can be used
	  *         to combine this $PL and another in a sequence.
	  */
	object followedBy extends FollowedBy[Self, Out] {
		def apply[T2](getNext: Out => Self[T2]): Self[T2] = handlerFactoryConverter.makeInstance(
			new HandlerFactory[In, T2] {
				def makeHandler(): Handler[In, T2] = {
					val handler1 = self.makeHandler()
					def getHandler2(h1Result: Out) = getNext(h1Result).makeHandler()
					new SequencedInStackHandler(handler1, getHandler2)
				}
			},
			s"$self.followedBy($getNext)"
		)
	}

	/** An intermediate object that can be used to create a Transformer from result of this Parser.
	  *
	  * Examples:
	  * {{{
	  *    val p1: $PL[A] = /* ... */
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
	  * An example of where this is useful is when an XML element contains some "dictionary" object
	  * at the beginning, followed by a sequence of "data" objects which reference the dictionary.
	  * For large sequences, combining them to a List (to use with $PL's `and` combiners) is undesireable;
	  * we can use this approach to avoid doing so.
	  *
	  * @return An intermediate object which has an `apply` and `flatMap` that can be used
	  *         to combine this $PL and a Transformer in a sequence.
	  */
	object followedByStream extends FollowedBy[({ type F[+T2] = Transformer[In, T2] })#F, Out] {
		def apply[T2](getTransformer: Out => Transformer[In, T2]): Transformer[In, T2] = new Transformer[In, T2] {
			override def toString = s"$self.followedByStream($getTransformer)"
			def makeHandler[End](next: Handler[T2, End]): Handler[In, End] = {
				val handler1 = self.makeHandler()
				def getHandler2(h1Result: Out): Handler[In, End] = getTransformer(h1Result).makeHandler(next)
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
	def interruptedBy(interrupter: HandlerFactory[In, Any]): Self[Out] = handlerFactoryConverter.makeInstance(
		new HandlerFactory[In, Out] {
			def makeHandler() = new InterrupterHandler(self.makeHandler(), interrupter.makeHandler())
		},
		s"$self.interruptedBy($interrupter)"
	)

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
	def beforeContext(matcher: ContextMatcher[StackElem, Any]): Self[Out] = {
		val interrupter = new ContextStackSplitter[In, StackElem, Any](matcher).map(Consumer.constant(true)).consumeFirst
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
	def expectInputs(expectations: List[(String, In => Boolean)]): Self[Out] = handlerFactoryConverter.makeInstance(
		new HandlerFactory[In, Out] {
			def makeHandler() = new ExpectationSequenceHandler(expectations, self.makeHandler())
		},
		s"$self.expectInputs($expectations)"
	)

	/** Creates a copy of this parser, but with a different `toString`
	  *
	  * @param name The new "name" (i.e. `toString`) for this parser
	  * @return A copy of this parser whose `toString` returns the given `name`
	  */
	def withName(name: String): Self[Out] = handlerFactoryConverter.makeInstance(self, name)
}

trait ParserCompanion[In, Self[+o] <: HandlerFactory[In, o]] { self =>
	def handlerFactoryConverter: FromHandlerFactory[In, Self]

	def from[Out](hf: HandlerFactory[In, Out]): Self[Out] = {
		handlerFactoryConverter.makeInstance(hf, hf.toString)
	}

	def constant[Out](value: Out): Self[Out] = from(new HandlerFactory[In, Out]{
		def makeHandler() = new ConstantHandler(value)
		override def toString = s"$self.constant($value)"
	})

	def oneOf[Out](parsers: Self[Out]*): Self[Out] = from(new HandlerFactory[In, Out]{
		def makeHandler() = new FallbackSetHandler(parsers.map(_.makeHandler()): _*)
	})
}



















