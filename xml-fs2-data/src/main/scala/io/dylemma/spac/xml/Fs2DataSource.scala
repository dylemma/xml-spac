package io.dylemma.spac.xml

import cats.MonadError
import cats.effect.SyncIO
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
  *        .through(Fs2DataSource.convertEvents)
  *    }
  *
  *    // could be replaced with
  *    val xmlStream2: Stream[IO, XmlEvent] = {
  *      Fs2DataSource[IO](charStream)
  *    }
  * }}}
  *
  * Essentially this helper just provides a convenient `apply` method that accepts
  * `String`, `Stream[F, Char]`, `Stream[F, String]`, or `Stream[F, fs2.data.xml.XmlEvent]`
  * to return a `Stream[F, XmlEvent]` which a `Parser` could then interact with.
  *
  * @group support
  */
object Fs2DataSource {

	/** Partial apply method for creating a `Stream[F, XmlEvent]` from some `source` */
	def apply[F[_]] = new Fs2DataSourcePartiallyApplied[F]
	/** Partial apply method for creating a `Stream[SyncIO, XmlEvent]` from some `source` */
	def syncIO = new Fs2DataSourcePartiallyApplied[SyncIO]

	class Fs2DataSourcePartiallyApplied[F[_]] {
		/** Given an implicit conversion from the `source` to a stream of fs2-data-xml XmlEvents,
		  * apply that conversion and further convert it to a stream of xml-spac XmlEvents.
		  *
		  * Via the default-available implicits, the source can be a `String`, a `Stream[F, String]`,
		  * or a `Stream[F, fs2.data.xml.XmlEvent]`. For String and String-Stream sources, a
		  * `Middleware` can be provided implicitly to override the default XmlEvent stream creation
		  * e.g. for the purposes of using custom normalization or entity resolution.
		  */
		def apply[S](source: S)(implicit S: ToFs2XmlEventStream[F, S]): Stream[F, XmlEvent] = {
			S(source) through convert
		}
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

	/** Typeclass allowing for automatic conversion from some `source` to a stream of `fs2.data.xml.XmlEvent`,
	  * typically via `fs2.data.xml.events` followed by post-processing defined by a `Middleware`,
	  * e.g. resolving references, namespaces, and normalization.
	  */
	trait ToFs2XmlEventStream[F[_], -S] {
		def apply(source: S): Stream[F, Fs2XmlEvent]
	}
	object ToFs2XmlEventStream {
		/** No-op conversion for an already-existing stream of fs2-data XmlEvents */
		implicit def identity[F[_]]: ToFs2XmlEventStream[F, Stream[F, Fs2XmlEvent]] = source => source

		/** Pipes a stream of string/char through the fs2-data-xml `events` and then the `middleware` pipe */
		implicit def fromRawXmlChunks[F[_], T](implicit F: MonadError[F, Throwable], T: CharLikeChunks[F, T], middleware: Middleware = Middleware.default): ToFs2XmlEventStream[F, Stream[F, T]] = {
			_ through events[F, T] through middleware.pipe
		}

		/** Pipes a single string (assumed to contain a complete XML document) through the fs2-data-xml `events` pipe and then the `middleware` pipe */
		implicit def fromRawXml[F[_]](implicit F: MonadError[F, Throwable], middleware: Middleware = Middleware.default): ToFs2XmlEventStream[F, String] = {
			source => Stream.emit(source) through events[F, String] through middleware.pipe
		}
	}

	/** Represents some post-processing on a stream of fs2-data-xml `XmlEvent`,
	  * e.g. piping the stream through a resolver and/or normalizer.
	  *
	  * This is how the `ToFs2XmlEventStream` typeclass allows you to override the stream creation
	  * behavior for the instances that use the `events` pipe themselves.
	  */
	trait Middleware {
		def pipe[F[_]](implicit F: MonadError[F, Throwable]): Pipe[F, Fs2XmlEvent, Fs2XmlEvent]
	}
	object Middleware {
		/** Default middleware which passes the input stream through `namespaceResolver`, then a default `referenceResolver`, then `normalize` */
		lazy val default: Middleware = new Middleware {
			def pipe[F[_]](implicit F: MonadError[F, Throwable]): Pipe[F, Fs2XmlEvent, Fs2XmlEvent] = {
				_ through namespaceResolver through referenceResolver() through normalize
			}
		}

		/** No-op middleware which returns the input stream unmodified */
		lazy val none: Middleware = new Middleware {
			def pipe[F[_]](implicit F: MonadError[F, Throwable]): Pipe[F, Fs2XmlEvent, Fs2XmlEvent] = {
				identity
			}
		}
	}
}
