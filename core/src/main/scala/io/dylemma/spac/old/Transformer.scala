package io.dylemma.spac.old

import java.io.Closeable
import java.util

import io.dylemma.spac.old.handlers._
import io.dylemma.spac.types.Stackable

import scala.language.higherKinds
import scala.util.{Failure, Success, Try}

/** A transformation function for a stream, i.e. `Stream => Stream`.
  * Transformers operate by wrapping a `downstream` handler with a new
  * handler that manages the transformation.
  *
  * Transformers themselves are immutable; they act as handler factories,
  * as a transformer requires a `downstream` handler in order to create its own handler.
  */
trait Transformer[-In, +B] extends (Any => Transformer[In, B]) { self =>
	def makeHandler[Out](next: Handler[B, Out]): Handler[In, Out]

	/** Transform a `src` resource with this transformer, yielding an iterator which will lazily
	  * consume the resource stream.
	  *
	  * @param src A resource which can be treated as a stream of `In` events
	  * @param cl Evidence that the `src` can be treated as a stream of `In` events
	  * @tparam R The resource type
	  * @return An iterator resulting from passing the `src` stream through this transformer.
	  */
	def transform[R](src: R)(implicit cl: ConsumableLike[R, In]): Iterator[B] with Closeable = {
		val inItr = cl.getIterator(src)
		var finished = false
		val buffer = new util.ArrayDeque[Try[B]](2)
		// we'll take vents from `inItr`, feed them through this transformer via `makeHandler`,
		// into a buffer which the returned iterator will consume
		val handler = makeHandler[Unit](new Handler[B, Unit] {
			override def isFinished: Boolean = finished
			override def handleInput(input: B): Option[Unit] = {
				buffer.push(Success(input))
				None
			}
			override def handleError(error: Throwable): Option[Unit] = {
				buffer.push(Failure(error))
				None
			}
			override def handleEnd(): Unit = {
				finished = true
			}
		})
		new Iterator[B] with Closeable {
			override def close(): Unit = inItr.close()
			override def hasNext: Boolean = {
				advanceUntilOutput()
				!buffer.isEmpty
			}
			override def next(): B = {
				advanceUntilOutput()
				if(buffer.isEmpty){
					throw new NoSuchElementException("next() after EOF")
				} else {
					// take the head of the buffer
					buffer.removeFirst().get
				}
			}

			private def advanceUntilOutput() = {
				// take elements from the iterator and feed them into the handler,
				// until either the handler finishes (typically due to an EOF),
				// or the handler pushes an event into the `buffer`.
				while(!handler.isFinished && buffer.isEmpty){
					if(inItr.hasNext){
						handler.handleInput(inItr.next)
					} else {
						handler.handleEnd()
					}
				}
			}
		}
	}

	/** Transformers count as functions that return themselves, so they can
	  * be used easily with Splitter's `throughT` method.
	  * @param v1 ignored
	  * @return this transformer
	  */
	def apply(v1: Any) = this

	// TODO: Document all of these methods

	def andThen[C](nextT: Transformer[B, C]): Transformer[In, C] = >>(nextT)
	def >>[C](nextT: Transformer[B, C]): Transformer[In, C] = new Transformer[In, C] {
		def makeHandler[Out](next: Handler[C, Out]): Handler[In, Out] = {
			self.makeHandler(nextT.makeHandler(next))
		}
		override def toString = s"$self >> $nextT"
	}

	def andThen[Out](end: Parser[B, Out]): Parser[In, Out] = >>(end)
	def >>[Out](end: Parser[B, Out]): Parser[In, Out] = new Parser[In, Out] {
		def makeHandler(): Handler[In, Out] = {
			self.makeHandler(end.makeHandler())
		}
		override def toString = s"$self >> $end"
	}

	def take(n: Int): Transformer[In, B] = andThen(Transformer.take(n))
	def takeWhile(p: B => Boolean): Transformer[In, B] = andThen(Transformer.takeWhile(p))
	def drop(n: Int): Transformer[In, B] = andThen(Transformer.drop(n))
	def dropWhile(p: B => Boolean): Transformer[In, B] = andThen(Transformer.dropWhile(p))
	def map[C](f: B => C): Transformer[In, C] = andThen(Transformer.map(f))
	def collect[C](pf: PartialFunction[B, C]): Transformer[In, C] = andThen(Transformer.collect(pf))
	def scan[S](init: S)(f: (S, B) => S): Transformer[In, S] = andThen(Transformer.scan(init)(f))
	def filter(p: B => Boolean): Transformer[In, B] = andThen(Transformer.filter(p))
	def flatten[B2](implicit ev: B <:< Iterable[B2]): Transformer[In, B2] = cast[Iterable[B2]].andThen(Transformer.flatten[B2])
	def parallel[In2 <: In, B2 >: B](other: Transformer[In2, B2]): Transformer[In2, B2] = Transformer.parallel(this :: other :: Nil)
	def parallelEither[In2 <: In, C](other: Transformer[In2, C]): Transformer[In2, Either[B, C]] = Transformer.parallelEither(this, other)
	def withFilter(p: B => Boolean): Transformer[In, B] = andThen(Transformer.filter(p))
	def unwrapSafe[T](implicit ev: B <:< Try[T]): Transformer[In, T] = {
		asInstanceOf[Transformer[In, Try[T]]].andThen(Transformer.unwrapSafe[T])
	}
	def upcast[In2 <: In, B2 >: B]: Transformer[In2, B2] = this
	def cast[B2](implicit ev: B <:< B2): Transformer[In, B2] = this.asInstanceOf[Transformer[In, B2]]

	@deprecated("It doesn't actually make sense to catch errors at the transformer level; use the `wrapSafe` on the appropriate Parser instead", "10/18/2020")
	def wrapSafe: Transformer[In, Try[B]] = andThen(Transformer.wrapSafe)

	def withSideEffect(effect: B => Any): Transformer[In, B] = andThen(Transformer.sideEffect(effect))

	def parseWith[Out](consumer: Parser[B, Out], setDebugName: Option[String] = None): Parser[In, Out] = new Parser[In, Out] {
		override def toString = setDebugName getOrElse s"$self.parseWith($consumer)"
		def makeHandler(): Handler[In, Out] = {
			self.andThen(consumer).makeHandler()
		}
	}
	def parseToList: Parser[In, List[B]] = parseWith(Parser.toList, Some(s"$this.parseToList"))
	def parseToMap[K, V](implicit ev: B <:< (K, V)) = cast[(K, V)].parseWith(Parser.toMap, Some(s"$this.parseToMap"))
	def parseFirst: Parser[In, B] = parseWith(Parser.first, Some(s"$this.parseFirst"))
	def parseFirstOption: Parser[In, Option[B]] = parseWith(Parser.firstOption, Some(s"$this.parseFirstOption"))
	def parseAsFold[Out](init: Out)(f: (Out, B) => Out): Parser[In, Out] = parseWith(Parser.fold(init, f), Some(s"$this.fold($init, $f)"))
	def parseForeach(f: B => Any): Parser[In, Unit] = parseWith(Parser.foreach(f), Some(s"$this.parseForeach($f)"))
	def sink: Parser[In, Unit] = parseForeach(_ => ())
}

object Transformer {
	def take[A](max: Int): Transformer[A, A] = new Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new TakeNHandler[A, Out](max, next)
		}
		override def toString = s"Take($max)"
	}

	def takeWhile[A](p: A => Boolean): Transformer[A, A] = new Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new TakeWhileHandler[A, Out](p, next)
		}
		override def toString = s"TakeWhile($p)"
	}

	def drop[A](numToDrop: Int): Transformer[A, A] = new Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new DropNHandler(numToDrop, next)
		}
		override def toString = s"Drop($numToDrop)"
	}

	def dropWhile[A](p: A => Boolean): Transformer[A, A] = new Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new DropWhileHandler(p, next)
		}
		override def toString = s"DropWhile($p)"
	}

	def filter[A](p: A => Boolean): Transformer[A, A] = new Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new FilteringHandler[A, Out](p, next)
		}
		override def toString = s"Filter($p)"
	}

	def flatten[A]: Transformer[Iterable[A], A] = new Transformer[Iterable[A], A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[Iterable[A], Out] = {
			new FlattenTransformerHandler[A, Out](next)
		}
		override def toString = "Flatten"
	}

	/** Transformer that feeds all inputs to all of the transformers in `ts`,
	  * passing all transformed inputs from each to a downstream handler.
	  * Each of the transformers should have the same transformed type `T`.
	  * If your transformers all have different types, consider mapping
	  * them to a sealed trait/coproduct/Either. If you have exactly two
	  * transformers to merge, use `MergeEither` instead.
	  *
	  * @param toMerge A list of Transformers to be run in parallel
	  * @tparam A The input type
	  * @tparam T The common "transformed" type
	  */
	def parallel[A, T](toMerge: List[Transformer[A, T]]): Transformer[A, T] = new Transformer[A, T] {
		def makeHandler[Out](next: Handler[T, Out]): Handler[A, Out] = {
			new ParallelTransformerHandler[A, T, Out](next, toMerge)
		}
	}

	/** Convenience version of `Parallel` for exactly two transformers of arbitrary types.
	  * Results from `t1` will be wrapped as `Left`, and results from `t2` will be wrapped as `Right`.
	  * The downstream handler will receive results of type `Either[T1, T2]`.
	  *
	  * @param left  The "left" transformer
	  * @param right The "right" transformer
	  * @tparam A The input type
	  * @tparam L The "left" transformer's "transformed" type
	  * @tparam R The "right" transformer's "transformed" type
	  */
	def parallelEither[A, L, R](left: Transformer[A, L], right: Transformer[A, R]): Transformer[A, Either[L, R]] = new Transformer[A, Either[L, R]] {
		def makeHandler[Out](next: Handler[Either[L, R], Out]): Handler[A, Out] = {
			new ParallelTransformerHandler[A, Either[L, R], Out](next, List(
				left.map(Left(_)),
				right.map(Right(_))
			))
		}
	}

	def map[A, B](f: A => B): Transformer[A, B] = new Transformer[A, B] {
		def makeHandler[Out](next: Handler[B, Out]): Handler[A, Out] = {
			new MappedTransformerHandler(f, next)
		}
		override def toString = s"Map($f)"
	}

	def collect[A, B](pf: PartialFunction[A, B]): Transformer[A, B] = new Transformer[A, B] {
		def makeHandler[Out](next: Handler[B, Out]): Handler[A, Out] = {
			new CollectHandler(pf, next)
		}
	}

	def scan[S, A](init: S)(f: (S, A) => S): Transformer[A, S] = new Transformer[A, S] {
		def makeHandler[Out](next: Handler[S, Out]): Handler[A, Out] = {
			new ScanningHandler(init, f, next)
		}
	}

	def unwrapSafe[A]: Transformer[Try[A], A] = new Transformer[Try[A], A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[Try[A], Out] = {
			new UnwrapSafeTransformerHandler(next)
		}
		override def toString = "UnwrapSafe"
	}

	def wrapSafe[A]: Transformer[A, Try[A]] = new Transformer[A, Try[A]] {
		def makeHandler[Out](next: Handler[Try[A], Out]): Handler[A, Out] = {
			new WrapSafeTransformerHandler(next)
		}
		override def toString = "WrapSafe"
	}

	def sideEffect[A](effect: A => Any): Transformer[A, A] = new Transformer[A, A] {
		def makeHandler[Out](next: Handler[A, Out]): Handler[A, Out] = {
			new SideEffectHandler(effect, next)
		}
		override def toString = s"SideEffect($effect)"
	}

	def sequenced[In: Stackable, T1, T2](consumer: Parser[In, T1], getTransformer: T1 => Transformer[In, T2]): Transformer[In, T2] = new Transformer[In, T2] {
		def makeHandler[Out](next: Handler[T2, Out]): Handler[In, Out] = {
			val handler1 = consumer.makeHandler()

			def getHandler2(h1Result: T1) = getTransformer(h1Result).makeHandler(next)

			new SequencedInStackHandler(handler1, getHandler2)
		}
		override def toString = s"Sequenced($consumer, $getTransformer)"
	}
}
