package io.dylemma.spac
package json

import cats.effect.SyncIO
import io.dylemma.spac.interop.fs2._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class JacksonInteropTests extends AnyFunSpec with Matchers with JsonParserBehaviors with JsonErrorHandlingBehaviors {
	describe("JSON support via Jackson") {
		describe("base behavior") {
			it should behave like jsonParserWithStringSource(JacksonSource.fromString(_))
		}
		describe("Exception 'spac' trace handling") {
			implicit val contextLineNumberSupport: ContextLineNumberSupport = ContextLineNumberSupport.Enabled
			it should behave like jsonErrorHandlingParserWithStringSource(
				JacksonSource.fromString(_),
				JacksonSource.fromString(_).toStream[SyncIO](),
			)
		}
	}

}
