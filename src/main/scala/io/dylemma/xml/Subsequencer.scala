//package io.dylemma.xml.experimental
//
//import javax.xml.stream.events.XMLEvent
//
///**
// * Created by Dylan on 6/28/2015.
// */
//trait Subsequencer[A, B] {
//	type State
//
//	def init: State
//	def fold(event: Event[A], state: State): (State, Subseq[B])
//}
//
//sealed trait Subseq[+A]
//object Subseq {
//	case class Start[A](value: A) extends Subseq[A]
//	case class Restart[A](value: A) extends Subseq[A]
//	case class Next[A](value: A) extends Subseq[A]
//	case object End extends Subseq[Nothing]
//	case object Skip extends Subseq[Nothing]
//}
//
//sealed trait Event[+A]
//object Event {
//	case class Next[A](value: A) extends Event[A]
//	case object End extends Event[Nothing]
//}
//
//// =======================================
//
//object IncreasingIntsSubsequencer extends Subsequencer[Int, Int] {
//	type State = Option[Int]
//	def init = None
//	def fold(event: Event[Int], state: Option[Int]) = state match {
//		case None =>
//			event match {
//				case Event.Next(value) => Some(value) -> Subseq.Start(value)
//				case Event.End => None -> Subseq.End
//			}
//		case Some(prev) =>
//			event match {
//				case Event.Next(value) if value > prev => Some(value) -> Subseq.Next(value)
//				case Event.Next(value) => Some(value) -> Subseq.Restart(value)
//				case Event.End => None -> Subseq.End
//			}
//	}
//}
//
//// =======================================
//
//case class XMLScopeSubsequencer(scopeFilter: List[String] => Boolean) extends Subsequencer[XMLEvent, XMLEvent] {
//	type State = List[String]
//	def init = Nil
//	def fold(event: Event[XMLEvent], state: List[String])
//}