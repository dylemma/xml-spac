package io.dylemma.spac
package impl

import cats.Applicative

import scala.collection.mutable

class ParserImpureBuilder[F[+_], In, To](val builder: mutable.Builder[In, To])(implicit F: Applicative[F]) extends Parser[F, In, To] {
	def step(in: In) = {
		this.builder += in
		F.pure { Right(this) }
	}
	def finish: F[To] = F.map(F.pure(this)) { _.builder.result() }
}
