package io.dylemma.spac

import cats.Monad
import cats.data.{Chain, NonEmptyChain}
import fs2.Pipe
import io.dylemma.spac.impl._
import org.tpolecat.typename.TypeName

import scala.annotation.tailrec

/** Primary "spac" abstraction which represents a transformation stage for a stream of data events
  *
  * Transformers effectively transform a stream of `In` events into a stream of `Out` events.
  * The actual stream handling logic is defined by a `Transformer.Handler`, which a `Transformer` is responsible for constructing.
  * Handlers may be internally-mutable, and so they are generally only constructed by other handlers.
  * Transformers themselves are immutable, acting as "handler factories", and so they may be freely reused.
  *
  * A transformer may choose to abort in response to any input event,
  * as well as emit any number of outputs in response to an input event or the EOF signal.
  *
  * @tparam In  The incoming event type
  * @tparam Out The outgoing event type
  * @groupname abstract Abstract Members
  * @groupname util Utility
  * @groupname combinator Transformation / Combinator Methods
  * @groupname parse Conversions to Parser
  * @groupprio abstract 0
  * @groupprio utility 1
  * @groupprio combinator 2
  * @groupprio parse 3
  */
trait Transformer[-In, +Out] {
	/** Transformer's main abstract method; constructs a new Handler representing this transformer's logic.
	  * Transformers are expected to be immutable, but Handlers may be internally-mutable.
	  *
	  * @group abstract
	  */
	def newHandler: Transformer.Handler[In, Out]

	/** Creates a copy of this transformer, but with a different `toString`
	  *
	  * @param name The new "name" (i.e. `toString` for this transformer
	  * @return A copy of this transformer whose `toString` returns the given `name`
	  * @group combinator
	  */
	def withName(name: String): Transformer[In, Out] = new TransformerWithName(this, name)

	/** Creates a new transformer which emits values according to the Emit transformation function `f`,
	  * where the inputs to `f` are the outputs emitted by this transformer.
	  *
	  * This is the low-level operation used by `map`, `filter`, and `collect`.
	  *
	  * @param f The Emit transformation function
	  * @tparam Out2 Output type of the resulting transformer
	  * @return A new transformer which alters the values emitted by this transformer according to `f`
	  * @group combinator
	  */
	def mapBatch[Out2](f: Emit[Out] => Emit[Out2]): Transformer[In, Out2] = new TransformerMapBatch(this, f)

	/** Creates a new transformer which applies the transformation function `f` to each of this transformer's outputs.
	  *
	  * @param f A transformation function
	  * @tparam Out2 The transformation output type
	  * @return The mapped transformer
	  * @group combinator
	  */
	def map[Out2](f: Out => Out2): Transformer[In, Out2] = mapBatch(_.map(f))

	/** Creates a new transformer which filters the outputs from this transformer.
	  *
	  * @param predicate A function which decides whether an output from this transformer should be emitted
	  *                  from the returned transformer. `true` means emit, `false` means skip.
	  * @return The filtered transformer
	  * @group combinator
	  */
	def filter(predicate: Out => Boolean): Transformer[In, Out] = mapBatch(_.filter(predicate))

	/** Alias for `filter`, used under the hood by for-comprehensions
	  *
	  * @param predicate The filtering function
	  * @return The filtered transformer
	  * @group combinator
	  */
	def withFilter(predicate: Out => Boolean): Transformer[In, Out] = filter(predicate)

	/** Creates a new transformer which filters and maps the outputs from this transformer
	  *
	  * @param pf Partial function responsible for the filtering and mapping of outputs from this transformer
	  * @tparam Out2 Result type of the `pf`
	  * @return The filteried and mapped transformer
	  * @group combinator
	  */
	def collect[Out2](pf: PartialFunction[Out, Out2]): Transformer[In, Out2] = mapBatch(_.collect(pf))

	/** Creates a new transformer which folds outputs from this transformer into a "state" which is emitted each time.
	  *
	  * @param init The initial "state"
	  * @param op   State update function; this is called for each `Out` emitted by this transformer, and the result
	  *             is emitted by the combined transformer in addition to becoming the next "state"
	  * @tparam Out2 The type of the scan "state"
	  * @return The new transformer
	  * @group combinator
	  */
	def scan[Out2](init: Out2)(op: (Out2, Out) => Out2): Transformer[In, Out2] = through(new TransformerScan(init, op))

	/** Creates a new transformer which sends inputs to both this transformer and the `right` transformer.
	  * Whenever either `this` or `right` emit a value, that value will be emitted from the returned transformer,
	  * wrapped as a `Left` or `Right` depending on which underlying transformer emitted it.
	  * For each individual input, the resulting values emitted by this transformer will be emitted before
	  * the resulting values emitted by the `right` transformer.
	  *
	  * @param right Another transformer
	  * @tparam In2  Contravariance-friendly version of `In`
	  * @tparam Out2 The output type of the `right` transformer
	  * @return The merged transformer
	  * @group combinator
	  */
	def mergeEither[In2 <: In, Out2](right: Transformer[In2, Out2]): Transformer[In2, Either[Out, Out2]] = TransformerMerge.either(this, right)

	@deprecated("This method is being renamed to `mergeEither`", "v0.9")
	def parallelEither[In2 <: In, Out2](right: Transformer[In2, Out2]) = mergeEither(right)

	/** Like `mergeEither`, but when both sides have a common output type.
	  * This is a less-roundabout way of doing `.mergeEither(right).map(_.merge)`.
	  * The same order-of-operations rules apply as with `mergeEither`, where this transformer "goes first" for each input.
	  *
	  * @param that Another transformer
	  * @tparam In2  Contravariance-friendly version of `In`
	  * @tparam Out2 Common output type between `this` and `that`
	  * @return The merged transformer
	  * @group combinator
	  */
	def merge[In2 <: In, Out2 >: Out](that: Transformer[In2, Out2]): Transformer[In2, Out2] = TransformerMerge(this, that)

	@deprecated("This method is being renamed to `merge`", "v0.9")
	def parallel[In2 <: In, Out2 >: Out](that: Transformer[In2, Out2]) = merge(that)

	/** Returns this transformer, but with less restricted `In` / `Out` types.
	  *
	  * @tparam In2  A subtype of `In`
	  * @tparam Out2 A supertype of `Out`
	  * @return This transformer (not a copy, it's actually literally `this`)
	  * @group combinator
	  */
	def upcast[In2 <: In, Out2 >: Out]: Transformer[In2, Out2] = this

	/** Returns this transformer, but with a different view of the `Out` type.
	  * The `Out <:< Out2` implicit evidence is used to make sure the `asInstanceOf` cast is safe.
	  * This is mostly useful when you know you have a transformer that yields a tuple or some kind of type constructor.
	  *
	  * @param ev
	  * @tparam Out2
	  * @return
	  * @group combinator
	  */
	def cast[Out2](implicit ev: Out <:< Out2): Transformer[In, Out2] = this.asInstanceOf[Transformer[In, Out2]]

	/** Attach this transformer to the `next` transformer, creating a single transformer that encapsulates the pair.
	  * Values emitted from this transformer will be passed as inputs to the `next` transformer,
	  * and the resulting outputs from the `next` transformer are emitted as outputs from the combined transformer.
	  *
	  * @param next
	  * @tparam Out2
	  * @return
	  * @group combinator
	  */
	def through[Out2](next: Transformer[Out, Out2]): Transformer[In, Out2] = {
		@inline def asChain(t: Transformer[_, _]) = t match {
			case TransformerStack(nec) => nec.toChain
			case _ => Chain.one(t.asInstanceOf[Transformer[Any, Any]])
		}
		TransformerStack(NonEmptyChain.fromChainUnsafe(asChain(this) ++ asChain(next)))
	}

	@deprecated("This method is being renamed to `through`", "v0.9")
	def andThen[Out2](next: Transformer[Out, Out2]) = through(next)

	@deprecated("Due to troubles with operator precedence and type inference, this operator is being phased out in favor of `through`", "v0.9")
	def >>[Out2](next: Transformer[Out, Out2]): Transformer[In, Out2] = through(next)

	/** Attach this transformer to a `parser`, creating a new parser that encapsulates the pair.
	  * Values emitted from this transformer will be passed as inputs to the `parser`,
	  * and the resulting output from the `parser` will be yielded as output by the combined parser.
	  *
	  * @param parser
	  * @tparam Out2
	  * @return
	  * @group parse
	  */
	def into[Out2](parser: Parser[Out, Out2]): Parser[In, Out2] = new TransformerIntoParser(this, parser)

	@deprecated("Use `into` instead", "v0.9")
	def parseWith[Out2](parser: Parser[Out, Out2]): Parser[In, Out2] = into(parser)

	@deprecated("Due to troubles with operator precedence and type inference, this operator is being phased out in favor of `:>`", "v0.9")
	def >>[Out2](parser: Parser[Out, Out2]): Parser[In, Out2] = into(parser)

	@deprecated("Use the single-argument version of `into`, then call `withName` on the resulting parser", "v0.9")
	def parseWith[Out2](parser: Parser[Out, Out2], setDebugName: Option[String]): Parser[In, Out2] = setDebugName match {
		case None => into(parser)
		case Some(name) => into(parser).withName(name)
	}

	/** Convenience for `this :> Parser.toList`
	  *
	  * @return
	  * @group parse
	  */
	def parseToList: Parser[In, List[Out]] = into(Parser.toList)

	/** Convenience for `this :> Parser.firstOpt`
	  *
	  * @return
	  * @group parse
	  */
	def parseFirstOpt: Parser[In, Option[Out]] = into(Parser.firstOpt)

	@deprecated("This method is being renamed to `parseFirstOpt`", "v0.9")
	def parseFirstOption = parseFirstOpt

	/** Convenience for `this :> Parser.fold(init)(f)`
	  *
	  * @param init
	  * @param f
	  * @tparam Out2
	  * @return
	  * @group parse
	  */
	def parseAsFold[Out2](init: Out2)(f: (Out2, Out) => Out2): Parser[In, Out2] = into(Parser.fold(init)(f))

	/** Convenience for `this :> Parser.tap(f)`
	  *
	  * @param f
	  * @return
	  * @group parse
	  */
	def parseTap(f: Out => Unit): Parser[In, Unit] = into(Parser.tap(f))

	@deprecated("This method is being renamed to `parseTap`", "v0.9")
	def parseForeach(f: Out => Any): Parser[In, Unit] = parseTap(in => f(in))

	/** Convenience for `this :> Parser.drain`
	  *
	  * @return
	  * @group parse
	  */
	def drain: Parser[In, Unit] = into(Parser.drain)

	@deprecated("This method is being renamed to `drain`", "v0.9")
	def sink = drain

	/** Applies this transformer's logic to an iterator, returning a new Iterator which yields values
	  * emitted by this transformer when run on the underlying `itr`.
	  *
	  * @param itr An iterator
	  * @return A wrapped version of `itr`, transformed via this transformer
	  * @group util
	  */
	def transform(itr: Iterator[In])(implicit pos: CallerPos): Iterator[Out] = new IteratorTransform(itr, this, SpacTraceElement.InParse("transformer", "transform", pos))

	def toPipe[F[_]](implicit pos: CallerPos, F: Monad[F]): Pipe[F, In, Out] = TransformerToPipe(this, SpacTraceElement.InParse("transformer", "toPipe", pos))
}

object Transformer {
	/** Extra transformer methods that had to be defined separately from the trait due to either `In` or `Out` needing to be invariant. */
	implicit class TransformerParsingOps[In, A](private val self: Transformer[In, A]) extends AnyVal {

		/** Convenience for `this :> Parser.first`
		  *
		  * @param A
		  * @return
		  * @group parse
		  */
		def parseFirst(implicit A: TypeName[A]): Parser[In, A] = self into Parser.first
	}

	/** Extra methods for transformers whose `Out` type is a Tuple2 */
	implicit class TransformerKVParsingOps[In, K, V](private val self: Transformer[In, (K, V)]) extends AnyVal {

		/** Convenience for `this :> Parser.toMap[K, V]`
		  *
		  * @return
		  * @group parse
		  */
		def parseToMap: Parser[In, Map[K, V]] = self.into(Parser.toMap)
	}

	trait Stateless[-In, +Out] extends Transformer[In, Out] with Handler[In, Out] {
		def newHandler: this.type = this
	}
	trait Handler[-In, +Out] {
		def step(in: In): (Emit[Out], Option[Handler[In, Out]])
		def finish(): Emit[Out]
		def unwind(err: Throwable): Throwable = err

		def stepMany[C[_], In2 <: In](_ins: C[In2])(implicit C: Unconsable[C]): (Emit[Out], Either[C[In2], Handler[In, Out]]) = {
			// fold inputs from `_ins` into this transformer, accumulating a buffer of `outs` and updating the transformer state along the way,
			// eventually returning the concatenation of all `outs`, and the final `transformer` state if the transformer is ready to continue,
			// or else the leftover unconsumed inputs if the transformer decided to end partway through
			var out: Emit[Out] = Emit.nil

			@tailrec def loop(current: Handler[In, Out], remaining: C[In2]): Either[C[In2], Handler[In, Out]] = {
				C.uncons(remaining) match {
					case Some((in, tail)) =>
						// next input, step the transformer
						val (emit, nextState) = current.step(in)
						out ++= emit
						nextState match {
							case None => Left(tail)
							case Some(cont) => loop(cont, tail)
						}
					case None =>
						// end of input, exit recursion by returning a Right with the current transformer
						Right(current)
				}
			}

			val endState = loop(this, _ins)
			out -> endState
		}

		/** Wraps this handler as a "top level" handler, which will inject a SpacTraceElement
		  * (representing the current input or the "EOF" signal)
		  * to any exception is thrown by this handler when calling its `step` or `finish` methods.
		  *
		  * Used internally by Transformers `transform` and `toPipe` methods.
		  */
		def asTopLevelHandler(caller: SpacTraceElement): Handler[In, Out] = new TopLevelTransformerHandler(this, caller)
	}

	/** Convenience for creating transformers whose input type is bound to `In`.
	  *
	  * This is particularly nice when the `Out` type can be inferred by the compiler, e.g.
	  * {{{
	  * Transformer[Int].op(i => Emit.one(i * 2))
	  * // versus
	  * Transformer.op[Int, Int](i => Emit.one(i * 2))
	  * }}}
	  */
	def apply[In] = new TransformerApplyBound[In]

	def identity[In]: Transformer[In, In] = new TransformerIdentity
	def op[In, Out](f: In => Emit[Out]): Transformer[In, Out] = new TransformerOp(f)
	def map[In, Out](f: In => Out): Transformer[In, Out] = op { in => Emit.one(f(in)) }
	def filter[In](f: In => Boolean): Transformer[In, In] = op { in => if (f(in)) Emit.one(in) else Emit.empty }
	def drop[In](n: Int): Transformer[In, In] = new TransformerDrop(n)
	def dropWhile[In](f: In => Boolean): Transformer[In, In] = new TransformerDropWhile(f)
	def take[In](n: Int): Transformer[In, In] = new TransformerTake(n)
	def takeWhile[In](f: In => Boolean): Transformer[In, In] = new TransformerTakeWhile(f)
	def tap[In](f: In => Unit): Transformer[In, In] = new TransformerTap(f)
	def spacFrame[In](elems: SpacTraceElement*): Transformer[In, In] = new TransformerSpacFrame[In](Chain(elems: _*))
}

/** Convenience version of the `Transformer` companion object,
  * which provides transformer constructors with the `In` type already specified.
  */
class TransformerApplyBound[In] {
	def identity: Transformer[In, In] = Transformer.identity
	def op[Out](f: In => Emit[Out]): Transformer[In, Out] = Transformer.op(f)
	def map[Out](f: In => Out): Transformer[In, Out] = Transformer.map(f)
	def filter(f: In => Boolean): Transformer[In, In] = Transformer.filter(f)
	def drop(n: Int): Transformer[In, In] = new TransformerDrop(n)
	def dropWhile(f: In => Boolean): Transformer[In, In] = new TransformerDropWhile(f)
	def take(n: Int): Transformer[In, In] = Transformer.take(n)
	def takeWhile(f: In => Boolean): Transformer[In, In] = Transformer.takeWhile(f)
	def tap(f: In => Unit): Transformer[In, In] = Transformer.tap(f)
	def spacFrame(elems: SpacTraceElement*): Transformer[In, In] = Transformer.spacFrame(elems: _*)
}
