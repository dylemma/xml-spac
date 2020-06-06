package io.dylemma.spac

import cats.{Monad, NotNull}

import scala.reflect.ClassTag


trait ConsumerK[F[+_], P[_[_], _, _], Ev[_]] {
	def apply[In, A: Ev](transformer: Transformer[F, In, A]): P[F, In, A]
}

object ConsumerK {

	def identity[F[+_]]: ConsumerK[F, Transformer, NotNull] = new ConsumerK[F, Transformer, NotNull] {
		def apply[In, A: NotNull](transformer: Transformer[F, In, A]): Transformer[F, In, A] = transformer
	}

	def first[F[+_]: MonadThrow]: ConsumerK[F, Parser, ClassTag] = new ConsumerK[F, Parser, ClassTag] {
		def apply[In, A: ClassTag](transformer: Transformer[F, In, A]): Parser[F, In, A] = transformer :> Parser.first
	}

	type OptParser[F[+_], In, A] = Parser[F, In, Option[A]]
	def firstOption[F[+_]: Monad]: ConsumerK[F, OptParser, NotNull] = new ConsumerK[F, OptParser, NotNull] {
		def apply[In, A: NotNull](transformer: Transformer[F, In, A]): OptParser[F, In, A] = transformer :> Parser.firstOpt
	}

	type ListParser[F[+_], In, A] = Parser[F, In, List[A]]
	def toList[F[+_]: Monad]: ConsumerK[F, ListParser, NotNull] = new ConsumerK[F, ListParser, NotNull] {
		def apply[In, A: NotNull](transformer: Transformer[F, In, A]): ListParser[F, In, A] = transformer :> Parser.toList
	}

}
