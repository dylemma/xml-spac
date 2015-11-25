package io.dylemma.xml

import language.higherKinds

/** Functor typeclass for transforming things that yield `Result`s,
	* e.g. Parsers, Matchers, and Transformers.
	*
	* @tparam M A parameterized type that generates result values
	*/
trait MapR[M[_]] {
	def mapR[A, B](ma: M[A], f: Result[A] => Result[B]): M[B]
}

/** Functor typeclass for transforming things that yield `Result`s
	* and have an additional "context" type param.
	* @tparam M A generic type with 2 parameters: `X` for context, and
	*           `A` for the types of results it generates
	*/
trait MapRC[M[_, _]] {
	def mapR[X, A, B](m: M[X, A], f: Result[A] => Result[B]): M[X, B]
}

/** Mixin that adds several result-related transformations to members of the `MapR`
	* and `MapRC` typeclasses.
	*/
trait MapROps extends MapRCOps {

	implicit class MapperOps[M[_], A](m: M[A])(implicit mapper: MapR[M]) {
		@inline def mapR[B](f: Result[A] => Result[B]): M[B] = mapper.mapR(m, f)
		def map[B](f: A => B): M[B] = mapR(_ map f)
		def flatMap[B](f: A => Result[B]): M[B] = mapR(_ flatMap f)
		def recover[A1 >: A](f: PartialFunction[Throwable, A1]): M[A1] = mapR(_ recover f)
		def recoverWith[A1 >: A](f: PartialFunction[Throwable, Result[A1]]): M[A1] = mapR(_ recoverWith f)
	}
}

// note: don't merge this directly into MapROps or else there'll be ambiguous implicits
trait MapRCOps {
	implicit class Mapper1ROps[M[_, _], X, A](m: M[X, A])(implicit mapper: MapRC[M]) {
		@inline def mapR[B](f: Result[A] => Result[B]) = mapper.mapR(m, f)
		def map[B](f: A => B): M[X, B] = mapR(_ map f)
		def flatMap[B](f: A => Result[B]): M[X, B] = mapR(_ flatMap f)
		def recover[A1 >: A](f: PartialFunction[Throwable, A1]): M[X, A1] = mapR(_ recover f)
		def recoverWith[A1 >: A](f: PartialFunction[Throwable, Result[A1]]): M[X, A1] = mapR(_ recoverWith f)
	}
}
