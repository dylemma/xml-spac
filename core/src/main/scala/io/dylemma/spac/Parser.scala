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
abstract class ParserLike[In, +Out, Self[+o] <: HandlerFactory[In, o]](
	protected implicit val handlerFactoryConverter: FromHandlerFactory[In, Self],
	protected implicit val stackable: Stackable[In]
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

	def interruptedBy(interrupter: Self[Any]): Self[Out] = handlerFactoryConverter.makeInstance(
		new HandlerFactory[In, Out] {
			def makeHandler() = new InterrupterHandler(self.makeHandler(), interrupter.makeHandler())
		},
		s"$self.interruptedBy($interrupter)"
	)

}

trait ParserCompanion[In, Self[+o] <: HandlerFactory[In, o]] { self =>
	implicit def handlerFactoryConverter: FromHandlerFactory[In, Self]

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



















