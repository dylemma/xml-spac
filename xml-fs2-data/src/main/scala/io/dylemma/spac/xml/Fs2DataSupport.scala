package io.dylemma.spac
package xml

import cats.Id
import cats.arrow.FunctionK
import cats.effect.{Sync, SyncIO}
import fs2.data.xml.{QName => Fs2DataQName}

object Fs2DataSupport {
	implicit val fs2DataQNameAsQName: AsQName[Fs2DataQName] = new AsQName[Fs2DataQName] {
		def name(n: Fs2DataQName): String = n.local
		def namespaceUri(n: Fs2DataQName): Option[String] = n.prefix
		def convert[N2](from: N2)(implicit N2: AsQName[N2]): Fs2DataQName = Fs2DataQName(N2.namespaceUri(from), N2.name(from))
		def equals[N2](l: Fs2DataQName, r: N2)(implicit N2: AsQName[N2]): Boolean = l.local == N2.name(r) && l.prefix == N2.namespaceUri(r)
	}

	/** Allows types which can be passed to `Fs2DataSource[F].apply` to also be passed to XmlParser's `parse` method.
	  * The general idea is that an `S` can be converted to an `fs2.Stream[F, fs2.data.xml.XmlEvent]`, and subsequently
	  * converted to a `fs2.Stream[F, io.dylemma.spac.xml.XmlEvent]` which an `XmlParser` can understand.
	  *
	  * By default, `S` can be `String`, `Stream[F, String]`, `Stream[F, Char]`, or `Stream[F, fs2.data.xml.XmlEvent]`.
	  * For the first three, the fs2-data-xml `events` pipe will be used, and you can provide an implicit `Fs2DataSource.Middleware`
	  * to inject extra pipes between the `events` and the eventual conversion to the xml-spac event model.
	  */
	implicit def fs2DataXmlSourceAsParsableF[F[_], S](
		implicit F: Sync[F],
		S: Fs2DataSource.ToFs2XmlEventStream[F, S]
	): Parsable[F, S, XmlEvent] = {
		Parsable.forFs2Stream[F, XmlEvent].contramapSource(Fs2DataSource[F](_))
	}

	implicit def fs2DataXmlUnsafeSyncSourceAsParsable[S](
		implicit S: Fs2DataSource.ToFs2XmlEventStream[SyncIO, S]
	): Parsable[cats.Id, S, XmlEvent] = {
		fs2DataXmlSourceAsParsableF[SyncIO, S].mapK(new FunctionK[SyncIO, cats.Id] {
			def apply[A](fa: SyncIO[A]): Id[A] = fa.unsafeRunSync()
		})
	}
}
