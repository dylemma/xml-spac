package io.dylemma.spac
package example

import cats.effect.{ExitCode, IO, IOApp}
import fs2.Pipe
import io.dylemma.spac.xml._

object Example10_Fs2 extends IOApp {
	val rawXml =
		"""<html>
		  |  <title>Hello</title>
		  |  <body>
		  |    <div class="cool stuff">Hello &amp; world</div>
		  |  </body>
		  |</html>""".stripMargin

	def run(args: List[String]): IO[ExitCode] = {

		/* Instead of importing `JavaxSupport._` for the implicits,
		 * you can just directly use the `JavaxSource` helper to construct a Stream.
		 */

		// just to show the types
		val bodyTextT: Transformer[XmlEvent, String] = Splitter.xml("html" \ "body").text
		val bodyTextP: Pipe[IO, XmlEvent, String] = bodyTextT.toPipe[IO]
		val xmlStream: fs2.Stream[IO, XmlEvent] = JavaxSource[IO](rawXml)

		// nice chained method call syntax
		JavaxSource[IO](rawXml)
			.through {
				Splitter.xml("html" \ "body").text.toPipe
			}
			.map(println(_))
			.compile
			.drain
			.as(ExitCode.Success)

	}
}
