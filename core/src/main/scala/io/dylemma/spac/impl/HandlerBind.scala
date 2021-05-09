package io.dylemma.spac
package impl

import scala.util.control.NonFatal

object HandlerBind {
	class Static[-In, Out](
		protected val self: Transformer.Handler[In, Out],
		protected val downstreamWrite: Transformer.BoundHandler[Out]
	) extends HandlerBind[In, Out] {
		protected def downstreamFinish(): Unit = {
			try downstreamWrite.finish()
			catch { case NonFatal(e) => self.bubbleUp(e) }
		}
		override def toString = s"HandlerBind.Static($self >> $downstreamWrite)"
	}

	class Dynamic[-In, Out](
		protected val self: Transformer.Handler[In, Out]
	) extends HandlerBind[In, Out] with Transformer.HandlerLinkage[Out] {
		protected var downstreamWrite: Transformer.HandlerWrite[Out] = Transformer.BoundHandler.noopAndContinue
		def setDownstream(newDownstream: Transformer.HandlerWrite[Out]): Unit = {
			downstreamWrite = newDownstream
		}
		protected def downstreamFinish(): Unit = downstreamWrite match {
			case bh: Transformer.BoundHandler[Out] =>
				try bh.finish()
				catch { case NonFatal(e) => self.bubbleUp(e) }
			case _ =>
				()
		}
		override def toString = s"HandlerBind.Dynamic($self >> $downstreamWrite)"
	}
}

trait HandlerBind[-In, Out] extends Transformer.BoundHandler[In] {
	protected def self: Transformer.Handler[In, Out]
	protected def downstreamWrite: Transformer.HandlerWrite[Out]
	protected def downstreamFinish(): Unit

	private var isStopped = false

	def push(in: In): Signal = {
		if (isStopped) Signal.Stop
		else {
			val signal =
				try self.push(in, downstreamWrite)
				catch {
					case NonFatal(e) =>
						isStopped = true
						self.bubbleUp(e)
				}
			if (signal.isStop) {
				isStopped = true
				// if the first transformer is early-ending the stream,
				// we need to inform the downstream that its input is finished
				downstreamFinish()
			}
			signal
		}
	}

	def finish(): Unit = {
		if (!isStopped) {
			try self.finish(downstreamWrite)
			catch {case NonFatal(e) => self.bubbleUp(e)}
			finally isStopped = true

			// also inform the downstream that the stream is ending
			downstreamFinish()
		}
	}
}
