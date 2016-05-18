package xsp

object TildeSyntax extends TildeSyntax
trait TildeSyntax {

	type ~[+A, +B] = (A, B)

	/** Syntactic sugar for matching nested tuples.
		* Example:
		* {{{
		*   val x = (((1, 'a'), "Hello"), new Foo)
		*   x match {
		*     case i ~ c ~ s ~ foo => ...
		*   }
		* }}}
		*/
	object ~ {
		def unapply[A, B](tup: (A, B)) = Some(tup)
	}

	/** Adds the `~` method to everything, to create tuple chains */
	implicit class AnyTildeChain[A](a: A){
		/** Create a pair out of `a` and `b`.
			* The operator can be chained, e.g.
			* {{{
			*   val x: (((Int, String), Foo), Bar) = 1 ~ "hi" ~ new Foo ~ new Bar
			* }}}
			* @param b
			* @tparam B
			* @return
			*/
		def ~[B](b: B) = (a, b)
	}

}
