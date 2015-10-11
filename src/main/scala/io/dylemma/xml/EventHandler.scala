package io.dylemma.xml

import javax.xml.stream.events.XMLEvent
import event._

/**
 * Created by Dylan on 6/28/2015.
 */
trait EventHandler[T, State] {
	def empty: State
	def fold(event: T, state: State): State
}

case class XmlContextState(tags: List[String]) {
	def push(tag: String) = XmlContextState(tags :+ tag)
	def pop = XmlContextState(tags dropRight 1)
}

class XmlContextHandler(tagsStartWith: List[String]) extends EventHandler[XMLEvent, XmlContextState] {

	def empty = XmlContextState(Nil)

	def /(tag: String) = new XmlContextHandler(tagsStartWith :+ tag)

	protected def forward(event: XMLEvent) = {
		println(s"forward event: $event")
	}

	protected def isMatchingState(state: XmlContextState) = {
		state.tags startsWith tagsStartWith
	}

	def fold(event: XMLEvent, state: XmlContextState) = event match {
		case StartElement(Name(tag), attributes) =>
			val newState = state push tag
			if (isMatchingState(newState)) {
				forward(event)
			}
			newState

		case EndElement(Name(tag)) =>
			if (isMatchingState(state)) forward(event)
			state.pop

		case _ if isMatchingState(state) =>
			forward(event)
			state

		case _ =>
			state
	}
}

object EventHandlerTest extends App {
	val source = """<thing>
		<a stuff="cool">
			This is some text
		</a>
		<b stuff="uncool">
			<stats>
				<stat name="asdf">Poop</stat>
				<stat name="fdsa">Goop</stat>
			</stats>
		</b>
	</thing>"""

	val handler = new XmlContextHandler("thing" :: "b" :: "stats" :: "stat" :: Nil)
	var state = handler.empty

	XMLEventSource(source) foreach { event =>
		state = handler.fold(event, state)
	}

}
