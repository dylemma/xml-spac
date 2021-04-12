package io.dylemma.spac
package xml

import cats.effect.{Sync, SyncIO}
import io.dylemma.spac.impl.ParsableByIterator
import javax.xml.XMLConstants
import javax.xml.namespace.QName
import javax.xml.stream.XMLInputFactory

/** Provides support for `javax.xml.stream` to be used with `io.dylemma.spac.xml`.
  *
  * Provides an implicit `Parsable` instance for the following types that can be plugged into a `javax.xml.stream.XMLInputFactory` to create an XMLStreamReader:
  *
  *  - `String`
  *  - `File`
  *  - `Resource[F, java.io.InputStream]`
  *  - `Resource[F, java.io.Reader]`
  *
  * A rough outline of the derivation/implementation of a `Parsable[F, Source]` is:
  * {{{
  *    Source -> Resource[F, XMLInputFactory] -> Stream[F, XmlEvent] -> Parsable[Source]
  * }}}
  */
package object spac_javax {

	/** Allows types which can be opened as a javax XMLStreamReader to be passed to an XmlParser's `parse` method.
	  * The "open as javax XMLStreamReader" logic is defined by the `IntoXmlStreamReader` typeclass.
	  */
	implicit def xmlStreamReadableAsParsableF[F[_], S](
		implicit F: Sync[F],
		S: IntoXmlStreamReader[F, S],
		factory: XMLInputFactory = JavaxSource.defaultFactory,
		chunkSize: ChunkSize = ChunkSize.default
	): Parsable[F, S, XmlEvent] = {
		Parsable.forFs2Stream[F, XmlEvent].contramapSource(JavaxSource[F](_))
	}

	implicit def xmlStreamReadableAsParsable[S](
		implicit S: IntoXmlStreamReader[SyncIO, S],
		factory: XMLInputFactory = JavaxSource.defaultFactory,
	): Parsable[cats.Id, S, XmlEvent] = new ParsableByIterator[S, XmlEvent] {
		protected def lendIterator[Out](source: S, f: Iterator[XmlEvent] => Out) = {
			JavaxSource.syncIO.iteratorResource(source).use(itr => SyncIO { f(itr) }).unsafeRunSync()
		}
	}

	/** Allows `javax.xml.namespace.QName` to be passed to name-based methods like `elem` and `attr` */
	implicit val javaxQNameAsQName: AsQName[QName] = new AsQName[QName] {
		def name(n: QName): String = n.getLocalPart
		def namespaceUri(n: QName): Option[String] = n.getNamespaceURI match {
			case null | XMLConstants.NULL_NS_URI => None
			case s => Some(s)
		}
		def convert[N2](from: N2)(implicit N2: AsQName[N2]): QName = from match {
			case q: QName => q // just a cast instead of constructing a new instance
			case n2 => new QName(null, N2.name(n2), N2.namespaceUri(n2).getOrElse(XMLConstants.DEFAULT_NS_PREFIX))
		}
		def equals[N2](n: QName, r: N2)(implicit N2: AsQName[N2]): Boolean = r match {
			case q: QName => n == q
			case n2 => (n.getLocalPart == N2.name(n2)) && (n.getNamespaceURI == N2.namespaceUri(n2).getOrElse(XMLConstants.NULL_NS_URI))
		}
	}

}
