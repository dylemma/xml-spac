//package io.dylemma.xml
//
//import javax.xml.stream.events.XMLEvent
//import event._
//
//sealed trait Event[+E]
//case class Next[E](value: E) extends Event[E]
//case object End extends Event[Nothing]
//
///**
// * Created by Dylan on 6/28/2015.
// */
//trait ProducingHandler[E, R] {
//	type State
//
//	def start: State
//	def fold(event: Event[E], state: State): State
//	def end(state: State): Production[R]
//}
//
//sealed trait Production[+T]
//case object Empty extends Production[Nothing]
//case class Produce[T](result: T) extends Production[T]
//
//object TextProducingHandler extends ProducingHandler[XMLEvent, String] {
//	type State = String
//	def start = ""
//	def fold(event: Event[XMLEvent], state: String) = event match {
//		case Next(Characters(s)) => state + s.trim
//		case _ => state
//	}
//	def end(state: String) = Produce(state)
//}
//
//object ListProducer {
//	def apply[A] = new ListProducer[A]
//}
//class ListProducer[A] extends ProducingHandler[A, List[A]] {
//	type State = List[A]
//	def start = Nil
//	def end(list: List[A]) = Produce(list)
//	def fold(event: Event[A], list: List[A]) = event match {
//		case Next(item) => list :+ item
//		case End => list
//	}
//}
//
//trait Accumulator[E, S] {
//	def start: S
//	def fold(event: E, scope: S): S
//}
//
//object XMLScopeAccumulator extends Accumulator[XMLEvent, List[String]] {
//	def start = Nil
//	def fold(event: XMLEvent, scope: List[String]) = event match {
//		case StartElement(Name(tag), _) => scope :+ tag
//		case EndElement(_) => scope dropRight 1
//		case _ => scope
//	}
//}
//
//case class ContextFilter2[E, Scope, InnerResult, Result](
//	scopeHandler: Accumulator[E, Scope],
//	scopeFilter: Scope => Boolean,
//	innerProducer: ProducingHandler[E, InnerResult],
//	outerProducer: ProducingHandler[InnerResult, Result]
//) {
//
//	type State = (Option[Scope], Option[innerProducer.State], outerProducer.State)
//
//	def start: State = (None, None, outerProducer.start)
//	def end(state: State) = outerProducer.end(state._3)
//	def fold(event: Event[E], state: State): State = {
//		val (oldScopeOpt, oldInnerState, oldOuterState) = state
//
//		val inScopeBefore = oldScopeOpt.fold(false)(scopeFilter)
//		val newScopeOpt = event match {
//			case Next(e) => Some(scopeHandler.fold(e, oldScopeOpt getOrElse scopeHandler.start))
//			case End => None
//		}
//		val inScopeAfter = newScopeOpt.fold(false)(scopeFilter)
//
//		print(event)
//		if (inScopeBefore) {
//
//			if (inScopeAfter) {
//				// inScope both Before && After => "still in scope"
//				println(" - still in scope")
//
//				assert(oldInnerState.isDefined, "Inner Producer State should be defined while the filter is in scope")
//				val newInnerState = Some(innerProducer.fold(event, oldInnerState.get))
//				(newScopeOpt, newInnerState, oldOuterState)
//
//			} else {
//				// inScope Before BUT NOT After => "leaving scope"
//				println(" - leaving scope")
//
//				assert(oldInnerState.isDefined, "Inner Producer State should be defined before the filter leaves its scope")
//				val nextInnerState = innerProducer.fold(event, oldInnerState.get)
//				val finalInnerState = innerProducer.fold(End, nextInnerState)
//				val newOuterState = innerProducer.end(finalInnerState) match {
//					case Empty => oldOuterState
//					case Produce(value) => outerProducer.fold(Next(value), oldOuterState)
//				}
//
//				(newScopeOpt, None, newOuterState)
//			}
//
//		} else {
//
//			if (inScopeAfter) {
//				// inScope After BUT NOT Before => "entering scope"
//				println(" - entering scope")
//
//				val newInnerState = Some(innerProducer.fold(event, innerProducer.start))
//				(newScopeOpt, newInnerState, oldOuterState)
//
//			} else {
//				// inScope neither Before OR After => "still not in scope"
//				println(" - still out of scope")
//
//				(newScopeOpt, oldInnerState, oldOuterState)
//			}
//		}
//
//	}
//}
//
//object ProductionTest extends App {
//	val source = """<thing>
//		<a stuff="cool">
//			This is some text
//		</a>
//		<b stuff="uncool">
//			<stats>
//				<stat name="asdf">Doop</stat>
//				<stat name="fdsa">Goop</stat>
//			</stats>
//		</b>
//	</thing>"""
//
//	val handler = new ContextFilter2(
//		XMLScopeAccumulator,
//		{ scope: List[String] =>
//			scope.startsWith("thing" :: "b" :: "stats" :: "stat" :: Nil)
//		},
//		TextProducingHandler,
//		ListProducer[String]
//	)
//	var state = handler.start
//	XMLEventSource(source) foreach { event =>
//		state = handler.fold(Next(event), state)
//	}
//	state = handler.fold(End, state)
//	val result = handler.end(state)
//	println(s"Final result: $result")
//}