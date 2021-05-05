package io.dylemma.spac
package impl

import io.dylemma.spac.Transformer.{BoundHandler, Handler, HandlerLinkage}

sealed trait TransformerStack[In, Out] extends Transformer[In, Out] {
	def newHandler: Handler[In, Out] = TransformerStack.buildHandler(this)
	override def through[X](next: Transformer[Out, X]): Transformer[In, X] = next match {
		case TransformerStack.Head(next) => TransformerStack.RCons(this, next)
		case TransformerStack.RCons(inits, last) => this.through(inits).through(last)
		case next => TransformerStack.RCons(this, next)
	}
}
object TransformerStack {
	case class Head[A, B](t: Transformer[A, B]) extends TransformerStack[A, B]
	case class RCons[A, X, B](init: TransformerStack[A, X], last: Transformer[X, B]) extends TransformerStack[A, B]

	def buildHandler[A, B](ts: TransformerStack[A, B]): Handler[A, B] = {
		def normalize[X](remaining: TransformerStack[A, X], tail: Normalized[X, B]): Normalized[A, B] = remaining match {
			case Head(t) => Cons(t.newHandler, tail)
			case RCons(init, last) => normalize(init, Cons(last.newHandler, tail))
		}
		val normalized = ts match {
			case Head(t) => End(t.newHandler)
			case RCons(init, last) => normalize(init, End(last.newHandler))
		}
		val (front, back) = normalized.build
		new Handler[A, B] {
			def push(in: A, out: Transformer.HandlerWrite[B]) = {
				back.setDownstream(out)
				front.push(in)
			}
			def finish(out: Transformer.HandlerWrite[B]): Unit = {
				back.setDownstream(out)
				front.finish()
			}
		}
	}

	private sealed trait Normalized[In, Out] {
		def build: (BoundHandler[In], HandlerLinkage[Out])
	}
	private case class End[A, B](head: Handler[A, B]) extends Normalized[A, B] {
		def build = {
			val proxy = Handler.bindVariableDownstream(head)
			proxy -> proxy
		}
	}
	private case class Cons[A, X, B](head: Handler[A, X], tail: Normalized[X, B]) extends Normalized[A, B] {
		def build = {
			val (tailProxy, end) = tail.build
			Handler.bindDownstream(head, tailProxy) -> end
		}
	}
}
