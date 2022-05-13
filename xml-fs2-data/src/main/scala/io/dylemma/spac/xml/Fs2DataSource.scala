package io.dylemma.spac.xml

import cats.MonadError
import fs2.data.text.CharLikeChunks
import fs2.data.xml.{XmlEvent => Fs2XmlEvent, _}
import fs2.{Pipe, Stream}

/** Provides helpers for creating FS2 streams of `io.dylemma.spac.xml.XmlEvent`,
  * using fs2-data-xml as the underlying event provider.
  *
  * Fs2-data is already pretty "plug-and-play", so at best this helper just removes
  * a few steps/pipes that you would have to manually call.
  *
  * For example:
  * {{{
  *    val charStream: Stream[IO, Char] = ???
  *
  *    // this...
  *    val xmlStream1: Stream[IO, XmlEvent] = {
  *      import fs2.data.xml._
  *      charStream
  *        .through(events)
  *        .through(namespaceResolver)
  *        .through(reverenceResolver())
  *        .through(normalize)
  *        .through(Fs2DataSource.convert)
  *    }
  *
  *    // could be replaced with
  *    val xmlStream2: Stream[IO, XmlEvent] = {
  *      Fs2DataSource.fromRawXmlStream(charStream)
  *    }
  * }}}
  *
  * Essentially this helper just provides a convenient `apply` method that accepts
  * `String`, `Stream[F, Char]`, `Stream[F, String]`, or `Stream[F, fs2.data.xml.XmlEvent]`
  * to return a `Stream[F, XmlEvent]` which an `XmlParser` could then interact with.
  *
  * @group support
  */
object Fs2DataSource {

	/** Converts a stream of raw XML data (such as Strings, Characters, or Bytes) to a stream of XmlEvents.
	  *
	  * Under the hood, this calls `events[F, A]` from `fs2-data-xml`, piped through the `cleanup` functions
	  * (i.e. `namespaceResolver, referenceResolver, normalize`), then converts the fs2-data based XML events
	  * to the SPaC model of `XmlEvent`
	  *
	  * @param rawXmlStream The raw XML data
	  * @param cleanup      Cleanup function used on the fs2-data XmlEvents. By default this will pass the events
	  *                     through `namespaceResolver through referenceResolver() through normalize`.
	  * @param A            Evidence that the `A` type can be treated as raw xml by fs2-data-xml
	  * @param F            Evidence that errors can be thrown in the `F` effect context
	  * @tparam F Stream effect type
	  * @tparam A The chunk type of the raw XML data, i.e. String, Char, or Byte
	  * @return A stream of SPaC `XmlEvent`s
	  */
	def fromRawXmlStream[F[_], A](rawXmlStream: Stream[F, A], cleanup: Cleanup = Cleanup)(implicit A: CharLikeChunks[F, A], F: MonadError[F, Throwable]): Stream[F, XmlEvent] = {
		rawXmlStream through events[F, A] through cleanup.pipe through convert[F]
	}

	/** Converts a raw XML string (expected to contain a complete XML document) to a stream of XmlEvents.
	  *
	  * @param rawXml  The raw XML data
	  * @param cleanup Cleanup function used on the fs2-data XmlEvents. By default this will pass the events
	  *                through `namespaceResolver through referenceResolver() through normalize`.
	  * @param F       Evidence that errors can be thrown in the `F` effect context
	  * @tparam F Stream effect type
	  * @return A stream of SPaC `XmlEvent`s
	  */
	def fromString[F[_]](rawXml: String, cleanup: Cleanup = Cleanup)(implicit F: MonadError[F, Throwable]): Stream[F, XmlEvent] = {
		fromRawXmlStream(Stream.emit(rawXml).covary[F], cleanup)
	}

	/** Pipe for converting `fs2.data.xml.XmlEvent` to `io.dylemma.spac.xml.XmlEvent`.
	  * This will be used under the hood of the `apply` and `syncIO` helpers,
	  * but is being made available to allow for manual stream creation if desired.
	  * The stream of fs2-data XmlEvents should ideally already be passed through
	  * a `referenceResolver` and `normalize` pipe before this one.
	  */
	def convert[F[_]]: Pipe[F, Fs2XmlEvent, XmlEvent] = _.collect {
		case start: Fs2XmlEvent.StartTag => new Fs2StartTagAsElemStart(start)
		case end: Fs2XmlEvent.EndTag => new Fs2EndTagAsElemEnd(end)
		case texty: Fs2XmlEvent.XmlTexty => new Fs2XmlTextyAsText(texty)
	}

	/** Represents some post-processing on a stream of fs2-data-xml `XmlEvent`,
	  * e.g. piping the stream through a resolver and/or normalizer.
	  *
	  * This is how the `ToFs2XmlEventStream` typeclass allows you to override the stream creation
	  * behavior for the instances that use the `events` pipe themselves.
	  */
	trait Cleanup {
		def pipe[F[_]](implicit F: MonadError[F, Throwable]): Pipe[F, Fs2XmlEvent, Fs2XmlEvent]
	}

	/** Default cleanup function which passes the input stream through `namespaceResolver` (with default args), then a default `referenceResolver`, then `normalize` */
	object Cleanup extends Cleanup {
		def pipe[F[_]](implicit F: MonadError[F, Throwable]): Pipe[F, Fs2XmlEvent, Fs2XmlEvent] = {
			_ through namespaceResolver through referenceResolver() through normalize
		}
	}

	/** No-op cleanup function which returns the input stream unmodified */
	object NoCleanup extends Cleanup {
		def pipe[F[_]](implicit F: MonadError[F, Throwable]): Pipe[F, Fs2XmlEvent, Fs2XmlEvent] = identity
	}
}
