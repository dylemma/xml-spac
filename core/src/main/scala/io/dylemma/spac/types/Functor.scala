package io.dylemma.spac.types

import io.dylemma.spac.{Consumer, ContextMatcher, Parser, SingleElementContextMatcher}

import scala.language.higherKinds
import scala.util.Try

/** Abstraction of `map` for container types
  * @tparam C
  */
trait Functor[C[_]] {
	def map[A, B](c: C[A], f: A => B): C[B]
}
object Functor {
	implicit object IdFunctor extends Functor[Id] {
		def map[A, B](c: A, f: A => B) = f(c)
	}
	implicit object OptionFunctor extends Functor[Option] {
		def map[A, B](c: Option[A], f: A => B) = c map f
	}
	implicit object ListFunctor extends Functor[List] {
		def map[A, B](c: List[A], f: A => B) = c map f
	}
	implicit object TryFunctor extends Functor[Try] {
		def map[A, B](c: Try[A], f: A => B) = c map f
	}
	implicit object ParserFunctor extends Functor[Parser] {
		def map[A, B](c: Parser[A], f: (A) => B): Parser[B] = c map f
	}
	implicit def consumerFunctor[In] = new Functor[({ type C[A] = Consumer[In, A] })#C] {
		def map[A, B](c: Consumer[In, A], f: (A) => B): Consumer[In, B] = c map f
	}
	implicit def contextMatcherFunctor[In] = new Functor[ContextMatcher] {
		def map[A, B](c: ContextMatcher[A], f: A => B): ContextMatcher[B] = c map f
	}
	implicit def singleElementContextMatcherFunctor[In] = new Functor[SingleElementContextMatcher] {
		def map[A, B](c: SingleElementContextMatcher[A], f: A => B): SingleElementContextMatcher[B] = c map f
	}
}
