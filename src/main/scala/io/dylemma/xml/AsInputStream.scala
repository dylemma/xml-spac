package io.dylemma.xml

import java.io.{ ByteArrayInputStream, FileInputStream, File, InputStream }

/** Typeclass that describes a Type (`T`) which can open and close an `InputStream` given
  * an instance of `T`, intended for use with the `javax.xml.stream` classes.
  *
  * Implementations may specify any `Resource` type they want, as long as it may
  * be treated as an InputStream, and be able to be closed later on. An example use case
  * would be if an implementation returned a wrapped InputStream, but the underlying
  * resource was the underlying stream. In that case, the class may want to define
  * `Resource` as `(InputStream, WrappedInputStream)`; the `resourceToStream` method
  * would return the `WrappedInputStream`, but the `closeResource` method would close
  * the underlying`InputStream`.
  */
trait AsInputStream[T] {
	type Resource

	def closeResource(res: Resource): Unit
	def openResource(descriptor: T): Resource
	def resourceToStream(res: Resource): InputStream
}

object AsInputStream {
	/** Implicit instance of `AsInputStream` for Files.
	  * Resources are opened as FileInputStreams and closed using the stream's own `close` method.
	  */
	implicit object FileCanBeInputSource extends AsInputStream[File] {
		type Resource = FileInputStream

		def closeResource(res: FileInputStream): Unit = res.close
		def openResource(file: File): FileInputStream = new FileInputStream(file)
		def resourceToStream(stream: FileInputStream): InputStream = stream
	}

	implicit object InputStreamCanBeInputSource extends AsInputStream[InputStream] {
		type Resource = InputStream
		def closeResource(res: InputStream): Unit = res.close()
		def openResource(res: InputStream): InputStream = res
		def resourceToStream(res: InputStream): InputStream = res
	}

	implicit object Func0AsInputStream extends AsInputStream[() => InputStream] {
		type Resource = InputStream
		def openResource(res: () => InputStream) = res()
		def closeResource(instr: InputStream) = instr.close()
		def resourceToStream(stream: InputStream) = stream
	}

	/** Implicit instance of `AsInputStream` for Strings.
	  * Resources are opened as InputStreams using the apache commons' IOUtils.
	  * Closing the string's stream is a no-op.
	  */
	implicit object StringCanBeInputSource extends AsInputStream[String] {
		type Resource = InputStream

		def closeResource(stream: InputStream): Unit = ()
		def openResource(str: String): InputStream = {
			new ByteArrayInputStream(str.getBytes)
		}
		def resourceToStream(stream: InputStream): InputStream = stream
	}

}