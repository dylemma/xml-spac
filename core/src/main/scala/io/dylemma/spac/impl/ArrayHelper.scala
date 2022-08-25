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

	def filterMapInPlace[A, B](array: Array[A], limit: Int)(f: A => Either[B, Option[A]]) = {
		// either returns the first B returned by the `f` function, or returns the new length limit for the array
		@tailrec def loop(i: Int, placeIndex: Int): Either[B, Int] = {
			if (i >= limit) Right(placeIndex)
			else {
				f(array(i)) match {
					case Left(b) => Left(b)
					case Right(None) => loop(i + 1, placeIndex)
					case Right(Some(a2)) =>
						array(placeIndex) = a2
						loop(i + 1, placeIndex + 1)
				}
			}
		}

		loop(0, 0)
	}

}
