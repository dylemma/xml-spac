package io.dylemma.xml.exp

import javax.xml.stream.events.XMLEvent

import io.dylemma.xml.XMLEventSource
import io.dylemma.xml.event._

/**
 * Created by Dylan on 6/28/2015.
 */
trait Stream[A] {
	def fold[R](folder: Folder[A, R]): R
}

trait Folder[A, R] {
	type State
	def start: State
	def fold(state: State, event: Event[A]): State
	def result(state: State): R
}

trait AltFolder[I, O] {
	def fold(input: Event[I]): AltFolder[I, O]
	def value: O
}

sealed trait Event[+A]
object Event {
	case class Next[A](value: A) extends Event[A]
	case object End extends Event[Nothing]
}

trait Accumulator[E, S] extends Folder[E, S] {
	type State = S
	def result(state: S) = state
}

trait Splitter[A] {
	type State
	def start: State
	def split(state: State, event: Event[A]): (State, Split)
}

sealed trait Split
object Split {
	case object Keep extends Split
	case object Skip extends Split
	case object End extends Split
	case object Reset extends Split
}

case class XMLScopeFilter(startingTags: List[String]) extends Function[List[String], Boolean] {
	def apply(tags: List[String]) = tags startsWith startingTags
}

// ===============================================

object XMLScopeAltFolder extends AltFolder[XMLEvent, List[String]] {

	def fold(input: Event[XMLEvent]) = StackedFolder(value).fold(input)
	def value = Nil

	private case class StackedFolder(stack: List[String]) extends AltFolder[XMLEvent, List[String]] {
		def fold(input: Event[XMLEvent]) = input match {
			case Event.End => StackedFolder(Nil)
			case Event.Next(StartElement(Name(tag), _)) => StackedFolder(stack :+ tag)
			case Event.Next(EndElement(_)) => StackedFolder(stack dropRight 1)
			case _ => this
		}
		def value = stack
	}
}



// ===============================================

object XMLScopeAccumulator extends Accumulator[XMLEvent, List[String]] {
	def start = Nil
	def fold(tags: List[String], event: Event[XMLEvent]) = event match {
		case Event.Next(StartElement(Name(tag), _)) => tags :+ tag
		case Event.Next(EndElement(_)) => tags dropRight 1
		case _ => tags
	}
}

// ===============================================

case class ScopeFilterSplitter[E, S](scopeAccumulator: Accumulator[E, S], scopeFilter: S => Boolean) extends Splitter[E] {
	type State = S
	def start = scopeAccumulator.start
	def split(state: S, event: Event[E]) = {
			val newState = scopeAccumulator.fold(state, event)

			val splitResult =
				if(scopeFilter(newState)) {
					if(event == Event.End) Split.End
					else Split.Keep
				}
				else {
					if(scopeFilter(state)) Split.End
					else Split.Skip
				}

			newState -> splitResult
	}
}

//case class TransformingFolder[E, R, T](splitter: Splitter[E], folder: Folder[E, R], outerFolder: Folder[R, T]) {
//
//	type State = (Option[splitter.State], Option[folder.State], outerFolder.State)
//	def start: State = (None, None, outerFolder.start)
//	def end(state: State) = state._3
//
//	def fold(state: State, event: E) = {
//		val (splitterStateOpt, folderStateOpt) = state
//
//		val splitterState = splitterStateOpt getOrElse splitter.start
//		val (newSplitterState, splitChoice) = splitter.split(splitterState, event)
//
//		splitChoice match {
//			case Split.
//		}
//	}
//}

// ================================================

object SplitterTest extends App {
	val source = """<thing>
		<a stuff="cool">
			This is some text
		</a>
		<b stuff="uncool">
			<stats>
				<stat name="asdf">Doop</stat>
				<stat name="fdsa">Goop</stat>
			</stats>
		</b>
	</thing>"""

	val splitter = ScopeFilterSplitter(XMLScopeAccumulator, XMLScopeFilter("foo" :: "b" :: "stats" :: "stat" :: Nil))
//	val splitter = ScopeFilterSplitter(XMLScopeAccumulator, XMLScopeFilter(Nil))
	var state = splitter.start
	XMLEventSource(source) foreach { event =>
		val (newState, split) = splitter.split(state, Event.Next(event))
		println(s"$split: $event")
		state = newState
	}
	val (lastState, split) = splitter.split(state, Event.End)
	println(s"$split: (end)")

}
