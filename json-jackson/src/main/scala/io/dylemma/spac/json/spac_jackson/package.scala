package io.dylemma.spac
package json

import cats.effect.Sync
import com.fasterxml.jackson.core.JsonFactory

package object spac_jackson {

	implicit def jacksonParserAsImpureJsonPull[F[+_], R](
		implicit F: Sync[F],
		intoJacksonJsonParser: IntoJacksonJsonParser[F, R],
		factory: JsonFactory = defaultJacksonParserFactory
	): ToPullable[F, R, JsonEvent] = {
		new ToPullable[F, R, JsonEvent] {
			def apply(source: R) = intoJacksonJsonParser(factory, source).map[F[*], Pullable[F, JsonEvent]](JacksonJsonPullable(_))
		}
	}

	lazy val defaultJacksonParserFactory: JsonFactory = new JsonFactory
}
