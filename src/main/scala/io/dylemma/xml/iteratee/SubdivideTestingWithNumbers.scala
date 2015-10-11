package io.dylemma.xml.iteratee

import scala.concurrent.{ Future, ExecutionContext }

import play.api.libs.iteratee.Enumeratee.{ Grouped, CheckDone }
import play.api.libs.iteratee._
import IterateeHelpers._

/**
 * Created by dylan on 10/10/2015.
 */
object SubdivideTestingWithNumbers {

	def main(args: Array[String]) {
		implicit val ec = Execution.trampoline
		val stream = Enumerator(1, 1, 0, 1, 0, 2, 2, 2, 0, 0)
//		val stream = Enumerator.eof[Int]
		val subdivideNonZeros = subdivide[Int](_ != 0).combineWith(Iteratee.getChunks)
		stream &> subdivideNonZeros |>>> Iteratee.foreach(println)
	}

}
