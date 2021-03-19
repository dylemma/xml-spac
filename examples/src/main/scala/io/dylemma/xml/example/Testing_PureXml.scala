package io.dylemma.xml.example

import cats.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import fs2.Pipe
import io.dylemma.spac.pure.Fs2Support._
import io.dylemma.spac.xml2.ContextMatcherSyntax._
import io.dylemma.spac.xml2.JavaxSupport._
import io.dylemma.spac.xml2.XmlExtensions._
import io.dylemma.spac.xml2.{XmlEvent, XmlSplitter}

object Testing_PureXml extends IOApp {
	val rawXml =
		"""<html>
		  |  <title>Hello</title>
		  |  <body>
		  |    <div class="cool stuff">Hello &amp; world</div>
		  |  </body>
		  |</html>""".stripMargin

	def run(args: List[String]): IO[ExitCode] = {

		val bodyTexts: Pipe[IO, XmlEvent, String] = XmlSplitter[IO, Unit]("html" \ "body").as.asText.toPipe

		val stream: fs2.Stream[IO, XmlEvent] = rawXml.toFs2Stream[IO, XmlEvent]




		stream.through(bodyTexts).map(println(_)).compile.drain.as(ExitCode.Success)

//		XmlPull[IO](rawXml).use { start =>
//			def loop(stream: XmlPull[IO]): IO[Unit] = stream.uncons.flatMap {
//				case None => IO.unit
//				case Some((head, tail)) =>
//					print(head)
//					loop(tail)
//			}
//			loop(start).as {
//				println()
//				ExitCode.Success
//			}
//		}
	}
}
