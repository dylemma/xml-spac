package io.dylemma.spac.syntax

import io.dylemma.spac.types.Functor

import scala.language.higherKinds

/** Provides implicits that add some syntactic convenience based on [[io.dylemma.spac.types.Functor]]s,
  * i.e. mapping operations.
  */
trait FunctorSyntax {

	/** Adds convenience mapping functions to "containers of containers"; notably, `Parser[Option[A]]`
	  *
	  * @param c A container of containers, e.g. a `List[List[Int]]` or a `Parser[Option[String]]`
	  * @param fFunctor A functor for the outer container's type (`F`)
	  * @param gFunctor A functor for the inner container's type (`G`)
	  * @tparam F The outer container type
	  * @tparam G The inner container type
	  * @tparam A The item type
	  */
	implicit class NestedFunctorOps[F[_], G[_], A](c: F[G[A]])(implicit fFunctor: Functor[F], gFunctor: Functor[G]) {

		/** Shorthand for `map(_.map(f))`
		  *
		  * @param f Mapping function for the inner items
		  * @tparam B The type of the mapped items
		  * @return
		  */
		def mapF[B](f: A => B): F[G[B]] = fFunctor.map(c, { g: G[A] => gFunctor.map(g, f) })
	}

}
