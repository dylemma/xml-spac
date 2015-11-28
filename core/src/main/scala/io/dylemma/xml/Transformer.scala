package io.dylemma.xml

import javax.xml.stream.events.XMLEvent
import scala.collection.generic.CanBuildFrom
import scala.concurrent.ExecutionContext
import scala.language.higherKinds

import play.api.libs.iteratee.{ Enumeratee, Iteratee }

/** Defines common functionality between `Transformer` and `TransformerWithContext`
	*/
trait TransformerCommon[A] {
	/** Result type for `transformWith` combinators */
	type T[B]
	/** Result type for `parseWith` combinators */
	type P[B]

	/** Consume results passed through this transformer to yield a single, final result.
		*
		* @param getIteratee A function that passes in the same ExecutionContext being used by
		*                    this transformer, and returns an Iteratee that consumes the results
		*                    of this transformer.
		* @tparam B The final result's type
		* @return A `Parser` whose result will be the final result returned by the iteratee.
		*/
	def parseWith[B](getIteratee: ExecutionContext => Iteratee[Result[A], Result[B]]): P[B]

	/** Transform the results passed through this transformer by applying an `Enumeratee` to its stream.
		*
		* @param getEnumeratee A function that returns the `Enumeratee` given an ExecutionContext.
		*                      The same ExecutionContext being used by this transformer will be passed
		*                      to `getEnumeratee`.
		* @tparam B The outgoing result type
		* @return A new transformer that passes along the `B` results output by the enumeratee.
		*/
	def transformWith[B](getEnumeratee: ExecutionContext => Enumeratee[Result[A], Result[B]]): T[B]

	/** Consume results passed through this transformer to yield a single, final result.
		*
		* @param iteratee The iteratee responsible for consuming the results
		* @tparam B The final result's type
		* @return A `Parser` whose result will be the final result returned by the `iteratee`.
		*/
	@inline def parseWith[B](iteratee: Iteratee[Result[A], Result[B]]): P[B] = parseWith(_ => iteratee)

	/** Transform the results passed through this transformer by applying an `Enumeratee` to its stream.
		*
		* @param enumeratee The `Enumeratee` responsible for transforming the stream
		* @tparam B The outgoing result type
		* @return A new transformer that passes along the `B` results output by the `enumeratee`
		*/
	@inline def transformWith[B](enumeratee: Enumeratee[Result[A], Result[B]]): T[B] = transformWith(_ => enumeratee)

	/** Create a parser that consumes and returns the first non-empty result of this transformer, if one exists.
		* If no first result exists, the parser will return an `Empty` instead.
		*/
	@inline def parseSingle: P[A] = parseWith { implicit ec => IterateeHelpers.consumeSingle[A] }

	/** Create a parser that consumes and returns the first non-empty result of this transformer, as an option.
		* If no first result exists, or the stream is full of `Empty` results, the parser will return a `None`.
		*/
	@inline def parseOptional: P[Option[A]] = parseWith { implicit ec => IterateeHelpers.consumeOptional[A] }

	/** Create a parser that consumes the results of this transformer as a List.
		* Empty results are ignored, while Error results will cause the entire list to be replaced
		* with that error.
		*/
	@inline def parseList: P[List[A]] = parseWith { implicit ec => IterateeHelpers.consumeList[A] }

	/** Create a parser that concatenates the results of this transformer according to some
		* implicitly-available `CanBuildFrom`. Empty results are ignored, while Error results
		* will cause the entire result to be replaced with that error.
		* @param t An implicit view of the results as a traversable
		* @param bf A builder from the traversable in `t` to some other result type
		* @tparam B The element type in the traversable from `t`
		* @tparam That The end result type
		*/
	@inline def parseConcat[B, That]()(implicit t: A => TraversableOnce[B], bf: CanBuildFrom[A, B, That]): P[That] = {
		parseWith { implicit ec => IterateeHelpers.consumeConcat }
	}

	/** Create a parser that runs the given `thunk` on the values of each successful
		* result in the stream. The parser's result will always be a `Unit`.
		*
		* @param thunk A side-effecting function on the elements in the stream
		*/
	@inline def foreach(thunk: A => Unit): P[Unit] = {
		parseWith { implicit ec => IterateeHelpers.runSideEffect(_ foreach thunk) }
	}

	/** Create a parser that runs the given `thunk` on every result in the stream.
		* The parser's result will always be a `Unit`.
		*
		* @param thunk A side-effecting function on the results in the stream
		*/
	@inline def foreachResult(thunk: Result[A] => Unit): P[Unit] = {
		parseWith { implicit ec => IterateeHelpers.runSideEffect(thunk) }
	}

	/** Create a new transformer that passes through the results from this transformer until
		* just after the `n`th Error result. If the `n`th error occurs, that result will be the
		* final result in the stream.
		*
		* @param n The number of of errors required to end the stream (aside from a normal EOF)
		*/
	@inline def takeThroughNthError(n: Int): T[A] = transformWith { implicit ec => IterateeHelpers.takeThroughNthError(n) }

	/** Create a new transformer that passes through the results from this transformer until
		* just after the first Error result. If the first error occurs, that result will be the
		* final result in the stream. Shortcut for `takeThroughNthError(1)`
		*/
	@inline def takeThroughFirstError: T[A] = takeThroughNthError(1)

	/** Create a new transformer that passes through the results from this transformer until
		* the `n`th error occurs. The `n`th error will be treated as an EOF and will not be
		* passed through.
		*
		* @param n The number of errors required to end the stream (aside from a normal EOF)
		*/
	@inline def takeUntilNthError(n: Int): T[A] = transformWith { implicit ec => IterateeHelpers.takeUntilNthError(n) }

	/** Create a new transformer that passes through the results from this transformer until
		* the first error occurs. The first error will be treated as an EOF and will not be
		* passed through. Shortcut for `takeUntilNthError(1)`
		*/
	@inline def takeUntilFirstError: T[A] = takeUntilNthError(1)

	/** Create a new transformer that folds over the results from this transformer using
		* the given `StreamScan` object. Error results from this transformer will cause the
		* StreamScan state to reset to its `init` state before being passed along. Result
		* values from the scan's `fold` and `finish` methods will be passed through.
		*
		* @param s A `StreamScan` that manages the internal logic of the fold.
		* @tparam B `s`'s output type
		*/
	@inline def scanResultsWith[B](s: StreamScan[A, B]) = transformWith { implicit ec => IterateeHelpers.scanResultsWith(s) }

	/** Create a new transformer that folds over the results from this transformer using
		* the given `StreamScan` object. Empty and Error results will be ignored by `s`.
		* Result values from the scan's `fold` and `finish` methods will be passed through.
		*
		* @param s A `StreamScan` that manages the internal logic of the fold.
		* @tparam B `s`'s output type
		*/
	@inline def scanWith[B](s: StreamScan[A, B]) = transformWith { implicit ec => IterateeHelpers.scanWith(s) }
}

/** Transforms a stream of XMLEvent into a stream of Result[A].
	* Transformers are generally created by combining a `Splitter` with a `Parser`.
	* They can be further transformed (e.g. to a stream of `Result[B]` instead) by
	* `transformWith`, or be turned into a parser via `parseWith`.
	*
	* @tparam A The type of results in the transformed stream
	*/
trait Transformer[A] extends TransformerCommon[A] { self =>

	/** Returns the `Enumeratee` representation of this transformer,
		* given an implicit ExecutionContext.
		*/
	def toEnumeratee(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Result[A]]

	type T[B] = Transformer[B]
	type P[B] = Parser[B]

	def parseWith[B](getIteratee: ExecutionContext => Iteratee[Result[A], Result[B]]): Parser[B] = {
		new Parser[B] {
			def toIteratee(implicit ec: ExecutionContext) = toEnumeratee transform getIteratee(ec)
		}
	}

	def transformWith[B](getEnumeratee: ExecutionContext => Enumeratee[Result[A], Result[B]]): Transformer[B] = new Transformer[B] {
		def toEnumeratee(implicit ec: ExecutionContext) = {
			self.toEnumeratee ><> getEnumeratee(ec)
		}
	}
}

object Transformer {
	implicit object TransformerMapR extends MapR[Transformer] {
		def mapR[A, B](ma: Transformer[A], f: Result[A] => Result[B]): Transformer[B] = new Transformer[B] {
			def toEnumeratee(implicit ec: ExecutionContext) = {
				ma.toEnumeratee ><> Enumeratee.map(f)
			}
		}
	}
}

/** Variant of the `Transformer` trait in which a "context" value must be passed in order to be useful.
	* Any parser created via `parseWith` will be a `ParserForContext`.
	* @tparam In The context type
	* @tparam A The type of results in the transformed stream
	*/
trait TransformerForContext[In, A] extends TransformerCommon[A] { self =>
	def toEnumeratee(in: In)(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Result[A]]

	type T[B] = TransformerForContext[In, B]
	type P[B] = ParserForContext[In, B]

	def parseWith[B](getIteratee: ExecutionContext => Iteratee[Result[A], Result[B]]): ParserForContext[In, B] = {
		new ParserForContext[In, B] {
			def toIteratee(context: In)(implicit ec: ExecutionContext) = toEnumeratee(context) transform getIteratee(ec)
		}
	}

	def transformWith[B](getEnumeratee: ExecutionContext => Enumeratee[Result[A], Result[B]]): TransformerForContext[In, B] = {
		new TransformerForContext[In, B] {
			def toEnumeratee(in: In)(implicit ec: ExecutionContext): Enumeratee[XMLEvent, Result[B]] = {
				self.toEnumeratee(in) ><> getEnumeratee(ec)
			}
		}
	}
}

object TransformerForContext {
	implicit object TransformerForContextMapper extends MapRC[TransformerForContext] {
		override def mapR[X, A, B](m: TransformerForContext[X, A], f: (Result[A]) => Result[B]): TransformerForContext[X, B] = new TransformerForContext[X, B] {
			def toEnumeratee(in: X)(implicit ec: ExecutionContext) = {
				m.toEnumeratee(in) ><> Enumeratee.map(f)
			}
		}
	}
}