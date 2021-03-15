package io.dylemma.spac
package types

import cats.Applicative

trait Stackable2[In, +Elem] {
	def interpretOne(input: In): StackInterpretation[In, Elem]

	def interpret[F[+_]: Applicative]: Transformer[F, In, Either[ContextChange[In, Elem], In]] = Transformer[F, In].op { in =>
		interpretOne(in) match {
			case StackInterpretation.NoChange => Emit.one(Right(in))
			case StackInterpretation.ChangedAfterInput(change) => Emit(Right(in), Left(change))
			case StackInterpretation.ChangedBeforeInput(change) => Emit(Left(change), Right(in))
		}
	}
}
