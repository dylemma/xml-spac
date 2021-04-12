package io.dylemma.spac

/** Used implicitly when creating certain `Parsable` instances to determine the "chunkSize" argument
  * for `Stream.fromBlockingIterator`.
  *
  * No implicit ChunkSize is available by default, but the implicit derivations that expect one
  * will default to `ChunkSize.default`, which uses a chunk size of 32.
  *
  * You can define a local `implicit val chunkSize = ChunkSize(n)` to override this default.
  *
  * @param i The chunk size as an integer
  */
case class ChunkSize(i: Int) extends AnyVal
object ChunkSize {
	def default = ChunkSize(32)
}
