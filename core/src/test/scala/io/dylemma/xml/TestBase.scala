package io.dylemma.xml

import org.scalatest.{ Matchers, FunSpec }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.time._
import play.api.libs.iteratee.Execution.trampoline

abstract class TestBase extends FunSpec with ScalaFutures with Matchers {
	implicit val trampolineExecutionContext = trampoline
	implicit val defaultPatienceConfig = PatienceConfig(Span(250, Milliseconds), Span(25, Milliseconds))
}
