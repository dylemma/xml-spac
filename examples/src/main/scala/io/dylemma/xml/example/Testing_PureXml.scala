package io.dylemma.xml.example

import cats.effect.{ExitCode, IO, IOApp}
import io.dylemma.spac.xml2.XmlPull

object Testing_PureXml extends IOApp {
	val rawXml =
		"""<html>
		  |  <title>Hello</title>
		  |  <body>
		  |    <div class="cool stuff">Hello &amp; world</div>
		  |  </body>
		  |</html>""".stripMargin

	def run(args: List[String]): IO[ExitCode] = {
		XmlPull[IO](rawXml).use { start =>
			def loop(stream: XmlPull[IO]): IO[Unit] = stream.uncons.flatMap {
				case None => IO.unit
				case Some((head, tail)) =>
					print(head)
					loop(tail)
			}
			loop(start).as {
				println()
				ExitCode.Success
			}
		}
	}
}
