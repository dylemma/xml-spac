package xsp

import javax.xml.stream.events.{StartElement, XMLEvent}

import xsp.handlers.XMLContextSplitterHandler


trait XMLContextSplitter[Context] extends Splitter[Context] with RMapOps[Context] { self =>

	/** Attempt to extract some `Context` from the given xml element stack.
		*
		* Implementation note:
		*
		* Handlers created by an XMLContextSplitter maintain an internal stack of StartElement
		* events, in order to represent their location within the XML document. We aim for O(1)
		* push, pop, head, and tail operations to support fast updating of the stack, as well
		* as support recursively-created context matchers (i.e. a head matcher checks the head
		* of the stack, and a tail matcher checks the tail of the stack). To accomplish these
		* performance characteristics, we use a (mutable) Array as a buffer, with an offset and
		* length value to indicate the window through which a matcher will view the buffer. The
		* head of the stack is the element at `stack(offset)`, and the tail of the stack is
		* passed by calling `matchContext(stack, offset + 1, length - 1)`.
		*
		* Note that although the array is technically mutable,
		* implementations should be pure functions, avoiding modifying the array.
		*
		* @param stack
		* @param offset
		* @param length
		* @return
		*/
	def matchContext(stack: Array[StartElement], offset: Int, length: Int): Result[Context]

	def through[P](parser: Parser[Context, P]): Transformer[XMLEvent, P] = {
		new Transformer[XMLEvent, P] {
			def makeHandler[Out](next: Handler[P, Out]): Handler[XMLEvent, Out] = {
				new XMLContextSplitterHandler(matchContext, parser, next)
			}
		}
	}
	type RMapped[B] = XMLContextSplitter[B]
	def mapResult[B](f: (Result[Context]) => Result[B]): RMapped[B] = new XMLContextSplitter[B] {
		def matchContext(stack: Array[StartElement], offset: Int, length: Int): Result[B] = {
			f(self.matchContext(stack, offset, length))
		}
	}
}

/** Specialization of ContextMatcher that allows combination with other matchers, forming a chain
	* which inductively matches the XML stack and combines individual results.
	*
	* @tparam A The type of the extracted context.
	*/
trait ChainingContextMatcher[+A] extends XMLContextSplitter[A] { self =>

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
	protected def matchSegment(stack: Array[StartElement], offset: Int, length: Int): Result[(A, Int)]

	def matchContext(stack: Array[StartElement], offset: Int, length: Int) = matchSegment(stack, offset, length).map(_._1)

	def /[B, AB](next: ChainingContextMatcher[B])(implicit c: ContextCombiner[A, B, AB]): ChainingContextMatcher[AB] = {
		new ChainingContextMatcher[AB] {
			protected def matchSegment(stack: Array[StartElement], offset: Int, length: Int) = {
				for {
					(leadMatch, leadLength) <- self.matchSegment(stack, offset, length)
					(nextMatch, nextLength) <- next.matchSegment(stack, offset + leadLength, length - leadLength)
				} yield c.combine(leadMatch, nextMatch) -> (leadLength + nextLength)
			}
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
	protected def matchSegment(stack: Array[StartElement], offset: Int, length: Int): Result[(A, Int)] = {
		if(length >= 1) matchElement(stack(offset)) map { _ -> 1 }
		else Result.Empty
	}

	// import alias this trait because of the long class name
	import xsp.{SingleElementContextMatcher => Match1}

	override type RMapped[B] = Match1[B]
	override def mapResult[B](f: (Result[A]) => Result[B]): Match1[B] = new Match1[B] {
		protected def matchElement(elem: StartElement) = f(self matchElement elem)
	}

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

	// common functionality for the [and]extract[q]name functions
	protected def expandMatch[B](f: (A, StartElement) => B): Match1[B] = new Match1[B] {
		protected def matchElement(elem: StartElement) = {
			self.matchElement(elem).map{ a => f(a, elem) }
		}
	}

	def extractQName = expandMatch { case (_, elem) => elem.getName }
	def extractName = expandMatch { case (_, elem) => elem.getName.getLocalPart }

	def andExtractQName = expandMatch { case (a, elem) => a -> elem.getName }
	def andExtractName = expandMatch { case (a, elem) => a -> elem.getName.getLocalPart }
}

object SingleElementContextMatcher {
	def predicate(f: StartElement => Boolean): SingleElementContextMatcher[Unit] = {
		new SingleElementContextMatcher[Unit] {
			protected def matchElement(elem: StartElement) = {
				if(f(elem)) Result.Success.unit else Result.Empty
			}
		}
	}

	def apply[A](f: StartElement => Option[A]): SingleElementContextMatcher[A] = {
		new SingleElementContextMatcher[A] {
			protected def matchElement(elem: StartElement) = Result fromOption f(elem)
		}
	}
}