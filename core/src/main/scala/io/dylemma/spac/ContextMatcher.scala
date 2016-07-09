package io.dylemma.spac

import javax.xml.stream.events.StartElement

trait ContextMatcher[+A] { self =>

	/** Attempt to extract some `Context` from the given xml element stack.
		*
		* Implementation note:
		*
		* Handlers created by an XMLContextSplitter maintain an internal stack of StartElement
		* events, in order to represent their location within the XML document. We aim for O(1)
		* push, pop, head, and tail operations to support fast updating of the stack, as well
		* as support recursively-created context matchers (i.e. a head matcher checks the head
		* of the stack, and a tail matcher checks the tail of the stack). To accomplish these
		* performance characteristics, we use an IndexedSeq as a buffer, with an offset and
		* length value to indicate the window through which a matcher will view the buffer. The
		* head of the stack is the element at `stack(offset)`, and the tail of the stack is
		* passed by calling `matchContext(stack, offset + 1, length - 1)`.
		*
		* @param stack
		* @param offset
		* @param length
		* @return
		*/
	def apply(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[A]

	def mapWith[B](f: Result[A] => Result[B]): ContextMatcher[B] = new ContextMatcher[B] {
		def apply(stack: IndexedSeq[StartElement], offset: Int, length: Int) = {
			f(self(stack, offset, length))
		}
	}
	def map[B](f: A => B) = mapWith(_ map f)
	def flatMap[B](f: A => Result[B]) = mapWith(_ flatMap f)
	def withFilter(f: A => Boolean) = mapWith(_ withFilter f)
}

/** Specialization of ContextMatcher that can chain with other ChainingContextMatchers,
	* creating a single matcher that combines the results of each member of the chain.
	* Internally, results are concatenated in the form of some `Chain` subtype, then
	* converted to some representative form (e.g. a Tuple, Unit, or a single value).
	*
	* @tparam A The type of the extracted context.
	* @tparam AChain Internal representation of `A` as a `Chain` subtype
	*/
trait ChainingContextMatcher[A, AChain <: Chain] extends ContextMatcher[A] { self =>

	/** Behaves like `apply` should, except that it additionally returns the length
		* of the matched segment. When creating a new matcher that chains off of this one,
		* the returned length should be added to the `offset` value and subtracted from the
		* `length` value of the next `matchContext` call.
		*
		* @return A result containing the chain representation of the matched segment of the stack,
		*         paired with the length of the matched segment. If the match was unsuccessful,
		*         returns `Result.Empty` or `Result.Error(err)` instead.
		*/
	protected def applyChain(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[(AChain, Int)]

	/** If there is a known, fixed minimum length for the stack in order for this matcher to
		* successfully match, this method will return a `Some` containing that minimum. This is
		* used as a hint when combining chained matchers, so that the combined chain matcher can
		* fail early when the stack is too small.
		*/
	protected def minStackLength: Option[Int]

	/** An object used to convert between the result type and its chain representation */
	protected def chainRep: ChainRep[A, AChain]

	def apply(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[A] = {
		applyChain(stack, offset, length).map { case (chain, _) => chainRep.fromChain(chain) }
	}

	override def mapWith[B](f: Result[A] => Result[B]): ChainingContextMatcher[B, Start ~ B] = {
		new ChainingContextMatcher[B, Start ~ B] {
			val minStackLength: Option[Int] = self.minStackLength
			protected val chainRep = new ChainRep[B, Start ~ B] {
				def toChain(t: B) = Start ~ t
				def fromChain(c: Start ~ B) = c.tail
			}
			protected def applyChain(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[(Start ~ B, Int)] = {
				val selfResult = self.applyChain(stack, offset, length)
				for {
					b <- f(selfResult.map(bl => self.chainRep.fromChain(bl._1)))
					len <- selfResult.map(_._2)
				} yield (Start ~ b, len)
			}
			override def apply(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[B] = {
				f(self(stack, offset, length))
			}
			override def toString = s"$self.mapWith($f)"
		}
	}
	override def map[B](f: A => B) = mapWith(_ map f)
	override def flatMap[B](f: A => Result[B]) = mapWith(_ flatMap f)
	override def withFilter(f: A => Boolean): ChainingContextMatcher[A, AChain] = {
		new ChainingContextMatcher[A, AChain] {
			protected val minStackLength: Option[Int] = self.minStackLength
			protected val chainRep: ChainRep[A, AChain] = self.chainRep
			protected def applyChain(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[(AChain, Int)] = {
				self.applyChain(stack, offset, length).withFilter {
					case (aChain, _) => f(chainRep.fromChain(aChain))
				}
			}
			override def toString = s"$self.withFilter($f)"
		}
	}

	/** Chain this matcher with the `next` matcher, returning a new matcher which represents the chain.
		*
		* @param next The next matcher in the chain
		* @param concat An implicitly-available object that knows how to combine the chain representations
		*               of this matcher and the `next` matcher
		* @param thatRep An implicitly-available object that serves as the `chainRep` for the resulting parser
		* @tparam B The result type of the `next` parser
		* @tparam BChain The chain representation of the `next` parser's result type
		* @tparam That The result type of the combined parser
		* @tparam ThatChain The chain representation of the combined parser's result type
		* @return A new matcher which combines this matcher and the `next` in a chain
		*/
	def \[B, BChain <: Chain, That, ThatChain <: Chain](next: ChainingContextMatcher[B, BChain])
		(implicit concat: ChainConcat[AChain, BChain, ThatChain], thatRep: ChainRep[That, ThatChain])
	: ChainingContextMatcher[That, ThatChain] =
	{
		new ChainingContextMatcher[That, ThatChain] {
			protected val minStackLength = for {
				amin <- self.minStackLength
				bmin <- next.minStackLength
			} yield amin + bmin
			protected val chainRep = thatRep
			protected def applyChain(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[(ThatChain, Int)] = {
				val stackBigEnough = minStackLength.fold(true)(length >= _)
				if(stackBigEnough){
					for {
						(selfChain, selfTaken) <- self.applyChain(stack, offset, length)
						(nextChain, nextTaken) <- next.applyChain(stack, offset + selfTaken, length - selfTaken)
					} yield concat(selfChain, nextChain) -> (selfTaken + nextTaken)
				} else {
					Result.Empty
				}
			}
			override def toString = s"$self / $next"
		}
	}
}


/** A further specialization of ChainingContextMatcher which matches exactly one element
	* from the XML stack.
	*/
trait SingleElementContextMatcher[A, AChain <: Chain] extends ChainingContextMatcher[A, AChain] { self =>

	/** Given a single element, return a match result (as a chain) */
	protected def applyElem(elem: StartElement): Result[AChain]
	protected val minStackLength = Some(1)
	protected def applyChain(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[(AChain, Int)] = {
		if(length < 1) Result.Empty
		else applyElem(stack(offset)).map(_ -> 1)
	}
	override def mapWith[B](f: (Result[A]) => Result[B]): SingleElementContextMatcher[B, Start ~ B] = {
		new SingleElementContextMatcher[B, Start ~ B] {
			protected val chainRep = new ChainRep[B, Start ~ B]{
				def toChain(b: B) = Start ~ b
				def fromChain(chain: Start ~ B): B = chain.tail
			}
			protected def applyElem(elem: StartElement): Result[Start ~ B] = {
				f(self.applyElem(elem).map(self.chainRep.fromChain)).map(Start ~ _)
			}
			override def toString = s"$self.mapWith($f)"
		}
	}
	override def map[B](f: A => B) = mapWith(_ map f)
	override def flatMap[B](f: A => Result[B]) = mapWith(_ flatMap f)
	override def withFilter(f: (A) => Boolean): SingleElementContextMatcher[A, AChain] = {
		new SingleElementContextMatcher[A, AChain] {
			protected val chainRep: ChainRep[A, AChain] = self.chainRep
			protected def applyElem(elem: StartElement): Result[AChain] = {
				self.applyElem(elem).withFilter{ aChain =>
					f(chainRep.fromChain(aChain))
				}
			}
			override def toString = s"$self.withFilter($f)"
		}
	}

	/** Create a new matcher that will take the result of the `other` matcher if this one fails */
	def |(other: SingleElementContextMatcher[A, AChain]): SingleElementContextMatcher[A, AChain] = {
		new SingleElementContextMatcher[A, AChain] {
			protected def chainRep: ChainRep[A, AChain] = self.chainRep
			protected def applyElem(elem: StartElement): Result[AChain] = {
				self.applyElem(elem) orElse other.applyElem(elem)
			}
			override def toString = s"$self | $other"
		}
	}

	/** Create a new matcher that will combine the results of both this matcher and the `other` matcher
		* on any given element. If either matcher fails, the combined result also fails. The combination
		* rules are similar to the rules on the `\` method, except that the resulting matcher only operates
		* on a single element, rather than a chain of elements.
		*/
	def &[B, BChain <: Chain, That, ThatChain <: Chain](other: SingleElementContextMatcher[B, BChain])
		(implicit concat: ChainConcat[AChain, BChain, ThatChain], thatRep: ChainRep[That, ThatChain])
	: SingleElementContextMatcher[That, ThatChain] = new SingleElementContextMatcher[That, ThatChain] {
		protected def chainRep: ChainRep[That, ThatChain] = thatRep
		protected def applyElem(elem: StartElement): Result[ThatChain] = {
			for {
				aChain <- self.applyElem(elem)
				bChain <- other.applyElem(elem)
			} yield concat(aChain, bChain)
		}
		override def toString = s"$self & $other"
	}
}

object SingleElementContextMatcher {
	def predicate(matcherName: String, f: StartElement => Boolean): SingleElementContextMatcher[Unit, Start] = {
		new SingleElementContextMatcher[Unit, Start] {
			protected val chainRep: ChainRep[Unit, Start] = ChainRep.UnitChainRep
			private val success = Result.Success(Start)
			protected def applyElem(elem: StartElement): Result[Start] = {
				if(f(elem)) success else Result.Empty
			}
			override def toString = matcherName
		}
	}

	def apply[A, AChain <: Chain](matcherName: String, f: StartElement => Result[A])(implicit rep: ChainRep[A, AChain])
	: SingleElementContextMatcher[A, AChain] = new SingleElementContextMatcher[A, AChain] {
		protected val chainRep: ChainRep[A, AChain] = rep
		protected def applyElem(elem: StartElement): Result[AChain] = {
			f(elem).map(chainRep.toChain)
		}
		override def toString = matcherName
	}
}