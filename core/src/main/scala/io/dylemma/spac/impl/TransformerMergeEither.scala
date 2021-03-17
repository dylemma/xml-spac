package io.dylemma.spac
package impl

import cats.Apply

class TransformerMergeEither[F[+_], -In, +A, +B](left: Transformer[F, In, A], right: Transformer[F, In, B])(implicit F: Apply[F]) extends Transformer[F, In, Either[A, B]] {
	def step(in: In): F[(Emit[Either[A, B]], Option[Transformer[F, In, Either[A, B]]])] = F.map2(left.step(in), right.step(in)) {
		case ((emitA, continueA), (emitB, continueB)) =>
			val emit: Emit[Either[A, B]] = emitA.map(Left(_)) ++ emitB.map(Right(_))
			val cont = (continueA, continueB) match {
				case (None, None) => None
				case (Some(l), Some(r)) => Some(new TransformerMergeEither(l, r))
				case (Some(l), None) => Some(l.map(Left(_)))
				case (None, Some(r)) => Some(r.map(Right(_)))
			}
			emit -> cont
	}
	def finish: F[Emit[Either[A, B]]] = F.map2(left.finish, right.finish) { (emitA, emitB) =>
		emitA.map(Left(_)) ++ emitB.map(Right(_))
	}
}