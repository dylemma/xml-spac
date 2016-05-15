package xsp

import xsp.{Chain => ~}

trait ResultChainTuple[C, T] {
	def toTuple(chain: C): Result[T]
}
object ResultChainTuple {
	implicit def forTuple2[A1, A2] = new ForTuple2[A1, A2]
	class ForTuple2[A1, A2] extends ResultChainTuple[Result[A1] ~ Result[A2], (A1, A2)] {
		def toTuple(chain: Result[A1] ~ Result[A2]): Result[(A1, A2)] = chain match {
			case r1 ~ r2 => for {
				a1 <- r1
				a2 <- r2
			} yield (a1, a2)
		}
	}

	implicit def forTuple3[A1, A2, A3] = new ForTuple3[A1, A2, A3]
	class ForTuple3[A1, A2, A3] extends ResultChainTuple[Result[A1] ~ Result[A2] ~ Result[A3], (A1, A2, A3)] {
		def toTuple(chain: Result[A1] ~ Result[A2] ~ Result[A3]): Result[(A1, A2, A3)] = chain match {
			case r1 ~ r2 ~ r3 => for {
				a1 <- r1
				a2 <- r2
				a3 <- r3
			} yield (a1, a2, a3)
		}
	}

	// TODO: use sbt-boilerplate to auto-generate instances for 2-22
}

// ====================================


