package io.dylemma.spac.syntax

import io.dylemma.spac.{Parser, ParserCombination}

/** Adds the `and` method to parsers (also available as the `~` operator).
  * A parser can be combined with another parser whose context type exists in its own context's type hierarchy.
  * This means that you can combine a `Parser[Any, ..]` and a `Parser[String, ..]` to get a `Parser[String, ..]`,
  * or a `Parser[String, ..]` and a `Parser[Any, ..]` to get a `Parser[String, ..]`, but you can't combine
  * a `Parser[String, ..]` and a `Parser[Int, ..]`.
  *
  * The rule of thumb is that for a `Parser[C1, ..]` and a `Parser[C2, ..]`, you can combine them if `C1` is a
  * subtype of `C2`, or if `C2` is a subtype of `C1`. The resulting combination will have a context type equal
  * to the "most specific" (i.e. the subtype) between `C1` and `C2`.
  */
trait ParserSyntax {

	implicit class ParserCombiningOps[C1, R1](p1: Parser[C1, R1]){
		def and[C2, R2](p2: Parser[C2, R2])(implicit ev: C2 <:< C1) = new ParserCombination.Combined2[C2, R1, R2](p1.mapContext(ev), p2)
		def ~[C2, R2](p2: Parser[C2, R2])(implicit ev: C2 <:< C1) = new ParserCombination.Combined2[C2, R1, R2](p1.mapContext(ev), p2)
		def and[C2 >: C1, R2](p2: Parser[C2, R2]) = new ParserCombination.Combined2[C1, R1, R2](p1, p2)
		def ~[C2 >: C1, R2](p2: Parser[C2, R2]) = new ParserCombination.Combined2[C1, R1, R2](p1, p2)
	}

}
