package xsp

import javax.xml.stream.events.StartElement

import xsp.ContextMatcher.ContextCombiner

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
	def matchContext(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[A]

	def mapResult[B](f: (Result[A]) => Result[B]): ContextMatcher[B] = new ContextMatcher[B] {
		def matchContext(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[B] = {
			f(self.matchContext(stack, offset, length))
		}
	}
	def map[B](f: A => B) = mapResult(_ map f)
}

object ContextMatcher {
	/** Typeclass for context combination rules.
		* - `Unit + A = A`
		* - `A + Unit = A`
		* - `A + B = (A, B)`
		*
		* @tparam A The left operand type
		* @tparam B The right operand type
		* @tparam AB The combined type
		*/
	trait ContextCombiner[-A, -B, +AB] {
		def combine(left: A, right: B): AB
	}
	object ContextCombiner extends LowPriorityContextCombinerImplicits {
		def apply[A, B, AB](f: (A, B) => AB): ContextCombiner[A, B, AB] = {
			new ContextCombiner[A, B, AB] {
				def combine(left: A, right: B): AB = f(left, right)
			}
		}
		implicit def getUnitAnyCombiner[A]: ContextCombiner[Unit, A, A] = apply { (l, r) => r }
	}

	trait LowPriorityContextCombinerImplicits extends LowerPriorityContextCombinerImplicits {
		implicit def getAnyUnitCombiner[A]: ContextCombiner[A, Unit, A] = ContextCombiner { (l, r) => l }
	}
	trait LowerPriorityContextCombinerImplicits {
		implicit def getABCombiner[A, B]: ContextCombiner[A, B, (A, B)] = ContextCombiner(Tuple2.apply)
	}
}

/** Specialization of ContextMatcher that allows combination with other matchers, forming a chain
	* which inductively matches the XML stack and combines individual results.
	*
	* @tparam A The type of the extracted context.
	*/
trait ChainingContextMatcher[+A] extends ContextMatcher[A] { self =>

	/** Behaves like `matchContext` should, except that it additionally returns the length
		* of the matched segment. When creating a new matcher that chains off of this one,
		* the returned length should be added to the `offset` value and subtracted from the
		* `length` value of the next `matchContext` call.
		*
		* @param stack
		* @param offset
		* @param length
		* @return
		*/
	protected def matchSegment(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[(A, Int)]

	def matchContext(stack: IndexedSeq[StartElement], offset: Int, length: Int) = matchSegment(stack, offset, length).map(_._1)

	def /[B, AB](next: ChainingContextMatcher[B])(implicit c: ContextCombiner[A, B, AB]): ChainingContextMatcher[AB] = {
		new ChainingContextMatcher[AB] {
			protected def matchSegment(stack: IndexedSeq[StartElement], offset: Int, length: Int) = {
				for {
					(leadMatch, leadLength) <- self.matchSegment(stack, offset, length)
					(nextMatch, nextLength) <- next.matchSegment(stack, offset + leadLength, length - leadLength)
				} yield c.combine(leadMatch, nextMatch) -> (leadLength + nextLength)
			}
			override def toString = s"$self / $next"
		}
	}

}

/** A further specialization of ChainingContextMatcher which matches exactly one element
	* from the XML stack.
	*
	* @tparam A The type of the extracted context.
	*/
trait SingleElementContextMatcher[+A] extends ChainingContextMatcher[A] { self =>

	protected def matchElement(elem: StartElement): Result[A]
	protected def matchSegment(stack: IndexedSeq[StartElement], offset: Int, length: Int): Result[(A, Int)] = {
		if(length >= 1) matchElement(stack(offset)) map { _ -> 1 }
		else Result.Empty
	}

	// import alias this trait because of the long class name
	import xsp.{SingleElementContextMatcher => Match1}

	override def mapResult[B](f: (Result[A]) => Result[B]): Match1[B] = new Match1[B] {
		protected def matchElement(elem: StartElement) = f(self matchElement elem)
	}
	override def map[B](f: A => B) = mapResult(_ map f)

	def &[B, AB](that: Match1[B])(implicit c: ContextCombiner[A, B, AB]): Match1[AB] = new Match1[AB] {
		protected def matchElement(elem: StartElement): Result[AB] = {
			for{
				a <- self.matchElement(elem)
				b <- that.matchElement(elem)
			} yield c.combine(a, b)
		}
	}

	def |[A1 >: A](that: Match1[A1]): Match1[A1] = new Match1[A1] {
		protected def matchElement(elem: StartElement): Result[A1] = {
			self.matchElement(elem) orElse that.matchElement(elem)
		}
	}

}

object SingleElementContextMatcher {
	def predicate(f: StartElement => Boolean, name: Option[String] = None): SingleElementContextMatcher[Unit] = {
		new SingleElementContextMatcher[Unit] {
			protected def matchElement(elem: StartElement) = {
				if(f(elem)) Result.Success.unit else Result.Empty
			}
			override def toString = name.getOrElse(super.toString)
		}
	}

	def apply[A](f: StartElement => Option[A], name: Option[String] = None): SingleElementContextMatcher[A] = {
		new SingleElementContextMatcher[A] {
			protected def matchElement(elem: StartElement) = Result fromOption f(elem)
			override def toString = name.getOrElse(super.toString)
		}
	}
}