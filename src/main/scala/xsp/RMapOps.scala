package xsp

// TODO: if it turns out this isn't helpful for dealing with Chain<=>Tuple ops, just remove it
trait RMapOps[A] {
	type RMapped[B]

	def mapResult[B](f: Result[A] => Result[B]): RMapped[B]

	def map[B](f: A => B): RMapped[B] = mapResult(_ map f)
	def flatMap[B](f: A => Result[B]): RMapped[B] = mapResult(_ flatMap f)
	def recover[A1 >: A](f: PartialFunction[Throwable, A1]): RMapped[A1] = mapResult(_ recover f)
	def recoverWith[A1 >: A](f: PartialFunction[Throwable, Result[A1]]): RMapped[A1] = mapResult(_ recoverWith f)
}
