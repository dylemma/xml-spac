package io.dylemma.spac

import io.dylemma.spac.core.Format
import io.dylemma.spac.handlers.{SplitOnMatchHandler, StackFormatSplitterHandler}
import scala.language.higherKinds

import scala.util.Try

trait Splitter[In, +Context] {
	def through[Out](joiner: Context => HandlerFactory[In, Out]): Transformer[In, Out]
}

class StackedFormatSplitter[Event, StackElem, Context](
	val stackFormat: Format.Aux[Event, StackElem],
	val matcher: ContextMatcher[StackElem, Context]
) extends Splitter[Event, Context] { self =>
	def through[P](joiner: Context => HandlerFactory[Event, P]): Transformer[Event, P] = new Transformer[Event, P] {
		def makeHandler[Out](next: Handler[P, Out]) = new StackFormatSplitterHandler(stackFormat, matcher, joiner, next)
		override def toString = s"$self( $joiner )"
	}
	override def toString = s"Splitter($matcher)"

	def as[Out](implicit joiner: Context => HandlerFactory[Event, Try[Out]]) = through(joiner)
}

trait SplitterApply[StackElem, Instance[+_]] {
	def apply[Context](matcher: ContextMatcher[StackElem, Context]): Instance[Context]
}

object Splitter {

	def apply[StackElem, Context, Instance[+_]](matcher: ContextMatcher[StackElem, Context])(implicit sa: SplitterApply[StackElem, Instance]): Instance[Context] = {
		sa(matcher)
	}

	def splitOnMatch[In, Context](matcher: PartialFunction[In, Context]): Splitter[In, Context] = {
		new Splitter[In, Context] {self =>
			override def toString = s"Splitter.splitOnMatch($matcher)"
			def through[Out](joiner: Context => HandlerFactory[In, Out]): Transformer[In, Out] = {
				new Transformer[In, Out] {
					override def toString = s"$self{ $joiner }"
					def makeHandler[A](downstream: Handler[Out, A]): Handler[In, A] = {
						new SplitOnMatchHandler(matcher, joiner, downstream)
					}
				}
			}
		}
	}

	def splitOnMatch[In](p: In => Boolean): Splitter[In, Any] = splitOnMatch[In, Any] {
		case in if p(in) => ()
	}
}