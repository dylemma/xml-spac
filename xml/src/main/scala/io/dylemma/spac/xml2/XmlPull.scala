package io.dylemma.spac.xml2

import cats.effect.Resource

// TODO: replace usage of this with the more generic Pullable trait
trait XmlPull[F[_]] {
	def uncons: F[Option[(XmlEvent, XmlPull[F])]]
}
object XmlPull {
	def apply[F[_]] = new XmlPullPartiallyApplied[F]
	class XmlPullPartiallyApplied[F[_]] {
		def apply[R](source: R)(implicit r: AsXmlPull[F, R]) = r(source)
	}
}

trait AsXmlPull[F[_], -R] {
	def apply(source: R): Resource[F, XmlPull[F]]
}
