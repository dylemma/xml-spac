package io.dylemma.spac.xml2.impl

import cats.Applicative
import cats.data.Chain
import io.dylemma.spac.Parser
import io.dylemma.spac.xml2.XmlEvent

class XmlTextCollector[F[+_]](buffer: Chain[String])(implicit F: Applicative[F]) extends Parser[F, XmlEvent, String] {
	def step(in: XmlEvent): F[Either[String, Parser[F, XmlEvent, String]]] = in.asText match {
		case None => F.pure(Right(this)) // continue unchanged
		case Some(textEvent) => F.pure(Right(new XmlTextCollector[F](buffer :+ textEvent.value))) // append to the buffer and continue
	}
	def finish: F[String] = F.pure(buffer.iterator.mkString) // smoosh the buffer into a single String
}
