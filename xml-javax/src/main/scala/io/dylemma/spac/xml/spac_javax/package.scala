package io.dylemma.spac
package xml

import cats.effect.{Resource, Sync}
import javax.xml.XMLConstants
import javax.xml.namespace.QName
import javax.xml.stream.XMLInputFactory

/** Provides support for `javax.xml.stream` to be used with `io.dylemma.spac.xml`.
  *
  * Provides an implicit `AsXmlPull` instance for the following types that can be plugged into a `javax.xml.stream.XMLInputFactory` to create an XMLStreamReader:
  *
  *  - `String`
  *  - `File`
  *  - `Resource[F, java.io.InputStream]`
  *  - `Resource[F, java.io.Reader]`
  *
  * Note that because javax's `XMLStreamReader` is internally mutable, the resulting XmlPull instances will not be referentially transparent.
  */
package object spac_javax {
	/** Allows types which can be opened as a javax XMLStreamReader to be passed to an XmlParser's `parse` method.
	  * The "open as javax XMLStreamReader" logic is defined by the `IntoXmlStreamReader` typeclass.
	  */
	implicit def xmlStreamReadableAsImpureXmlPull[F[+_], R](implicit F: Sync[F], intoXmlStreamReader: IntoXmlStreamReader[F, R], factory: XMLInputFactory = defaultFactory): ToPullable[F, R, XmlEvent] = new ToPullable[F, R, XmlEvent] {
		def apply(source: R): Resource[F, Pullable[F, XmlEvent]] = {
			val readerResource: Resource[F, WrappedStreamReader] = intoXmlStreamReader(factory, source)
				.flatMap[F[*], WrappedStreamReader] { streamReader => // the explicit type hint seems to be necessary on scala 2.12
					Resource.fromAutoCloseable(F.delay {new WrappedStreamReader(streamReader)})
				}
			WrappedStreamReader.resourceAsXmlPull(readerResource)
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

	lazy val defaultFactory: XMLInputFactory = {
		val factory = XMLInputFactory.newInstance
		factory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)
		factory.setProperty(XMLInputFactory.IS_SUPPORTING_EXTERNAL_ENTITIES, false)
		factory
	}
}
