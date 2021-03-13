package io.dylemma.spac.xml2

import io.dylemma.spac._

object XmlExtensions extends XmlExtensions
trait XmlExtensions {
	implicit class XmlSplitterConsumerOps[F[+_]: MonadThrow, P[_[_], _, _], Ev[_], C](base: SplitterConsumerOps[F, XmlEvent, P, Ev, C]) {
		def attr[N: AsQName](name: N)(implicit ev: Ev[String]): P[F, XmlEvent, String] = base.into { _ => XmlParser.forMandatoryAttribute(name) }
		def asText(implicit ev: Ev[String]): P[F, XmlEvent, String] = base.into { _ => XmlParser.forText }
	}
}
