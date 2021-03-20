package io.dylemma.spac.impl

import scala.annotation.tailrec

object ArrayHelper {
	// takes the value at `arr(leftIndex)`, moves it into `arr(intoIndex)`,
	// and shifts everything in between to the left by one
	def placeAndShiftLeft[A](arr: Array[A], leftIndex: Int, intoIndex: Int): Unit = {
		@tailrec def loop(toPlace: A, rightIndex: Int): Unit = {
			if (rightIndex > leftIndex) {
				val replaced = arr(rightIndex)
				arr(rightIndex) = toPlace
				loop(replaced, rightIndex - 1)
			} else if (rightIndex == leftIndex) {
				arr(leftIndex) = toPlace
			} else {
				throw new IllegalArgumentException(s"$rightIndex was not >= $leftIndex")
			}
		}
		loop(arr(leftIndex), intoIndex)
	}
}
