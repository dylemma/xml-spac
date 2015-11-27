package io.dylemma.xml.example

import scala.concurrent.Future

import io.dylemma.xml.Parser.MultiplexedParser
import io.dylemma.xml.Result.Error
import io.dylemma.xml.Result._
import io.dylemma.xml._
import ParsingDSL._
import play.api.libs.iteratee.Enumeratee.CheckDone
import play.api.libs.iteratee.{Iteratee, Enumeratee, Enumerator}
import play.api.libs.iteratee.Execution.Implicits.trampoline
import play.api.libs.iteratee.Input

/**
 * Created by dylan on 11/24/2015.
 */
object DefinitionList extends App {

	val rawXml = """<multimap>
		| <entrylist>
		|   <key>hello</key>
		|   <value>5</value>
		|   <value>2</value>
		|   <key>goodbye</key>
		|   <key>floopy</key>
		|   <value>3</value>
		|   <value>4</value>
		|   <value>5</value>
		|   <key>orange</key>
		|   <value>3</value>
		|   <value>4</value>
		|   <value>5</value>
		| </entrylist>
		| <anotherThing>Hello</anotherThing>
		|</multimap>""".stripMargin

	val keyOrValueParser = new MultiplexedParser[String, Either[String, Int]]({
		case "key" => (* % Text).map(Left(_))
		case "value" => (* % Text).map(_.toInt).flatMap{
			case 4 => Error(new Exception("I don't like 4's"))
			case i => Success(i)
		}.map(Right(_))
	})

	case class Entry(key: String, values: List[Int])

	val firstPart = Root / "multimap" / ("key" | "value").extractName through keyOrValueParser

	def collectSuccesses[T] = Enumeratee.collect[Result[T]]{ case Success(t) => t }
	def collectErrors[T] = Enumeratee.collect[Result[T]]{ case e: Error => e }

	type KV = Either[String, Int]
	type RKV = Result[KV]

	/*
	I want an Enumeratee[Result[A], X]
	that will take in Success results and pass them to a Parser[A]
	and ignore any Empty results
	but if an Error result is found, cause the underlying parser to emit an error
	 */
	val transformer = Enumeratee.zipWith(
		/*
		The problem here is that if there is no error, Iteratee.head will
		end up consuming the whole stream, so the actual consumer won't get
		to repeat on the remaining actual results.
		 */
		collectErrors[KV] &>> Iteratee.head,
		collectSuccesses[KV] &>> (for {
				_ <- Enumeratee.takeWhile[KV](_.isRight) &>> Iteratee.ignore
				key <- Iteratee.head[KV].map(_.flatMap(_.left.toOption))
				values <- Enumeratee.takeWhile[KV](_.isRight) ><> Enumeratee.map(_.right.get) &>> Iteratee.getChunks
			} yield {
				Result.fromOption(key) map {Entry(_, values)}
			})
	){(errOpt, actualResult) => errOpt getOrElse actualResult}

	val enumerateEntries = Enumeratee.grouped[KV]{
		for {
			_ <- Enumeratee.takeWhile[KV](_.isRight) &>> Iteratee.ignore
			key <- Iteratee.head[KV].map(_.flatMap(_.left.toOption))
			values <- Enumeratee.takeWhile[KV](_.isRight) ><> Enumeratee.map(_.right.get) &>> Iteratee.getChunks
		} yield {
			Result.fromOption(key) map { Entry(_, values) }
		}
	}

//	val secondPart = firstPart andThenR (recovery ><> takeSuccess ><> enumerateEntries)
//	val secondPart = firstPart andThenR (Enumeratee.grouped(transformer))

	def innerResultCheck(f: KV => Boolean): (RKV=>Boolean) = {
		case Success(kv) => f(kv)
		case _ => true
	}

	val floop = Enumeratee.grouped[RKV] {
		for {
			_ <- Enumeratee.takeWhile[RKV](innerResultCheck(_.isRight)) &>> Iteratee.ignore
			key <- Iteratee.head[RKV]//.map(_.map(_.collect{ case Left(k) => k }))
			values <- Enumeratee.takeWhile(innerResultCheck(_.isRight)) &>> Iteratee.getChunks
		} yield {
			val keyResult = Result.fromOption(key).flatten.collect{ case Left(k) => k }
			val valuesResult = Result.list(values.map(_ collect { case Right(v) => v }))
			println(s" -- Key = $key")
			println(s" -- Values = $values")
			for {
				key <- keyResult
				values <- valuesResult
			} yield Entry(key, values)
		}
	}

	sealed trait FoldThing[+T]
	case class Continue[T](value: T) extends FoldThing[T]
	case object Finish extends FoldThing[Nothing]

	// Treat every input as a Continue
	// Treate EOF as a 'Finish' + Continue
	def upgradeToFoldThing[From] = Enumeratee.mapInputFlatten[From].apply[FoldThing[From]] {
		case Input.EOF => Enumerator.enumInput(Input.El[FoldThing[From]](Finish)).andThen(Enumerator.enumInput(Input.EOF))
		case other => Enumerator.enumInput(other.map(Continue(_)))
	}

	trait FolderFunction[State, A, B] {
		def init: State
		def fold(state: State, input: A): (State, Result[B])
		def finish(state: State): Result[B]
	}

	object EntryFolder extends FolderFunction[(Option[String], List[Int]), Either[String, Int], Entry] {
		def init = None -> Nil
		def finish(state: (Option[String], List[Int])) = {
			Result.fromOption(state._1).map(Entry(_, state._2))
		}
		def fold(state: (Option[String], List[Int]), input: Either[String, Int]) = input match {
			case Left(key) =>
				state match {
					// emit the current state and start a new entry
					case (Some(oldKey), values) => (Some(key), Nil) -> Result{ Entry(oldKey, values) }
					// start a new entry
					case (None, values) => (Some(key), values) -> Empty
				}
			case Right(value) =>
				state match {
					// add the value to the current state
					case (Some(key), values) => (Some(key), values :+ value) -> Empty
					// ignore values that appear before a key
					case (None, values) => (None, Nil) -> Empty
				}
		}
	}


	def makeScannerThingy[State, A, B](f: FolderFunction[State, A, B]) = {
		upgradeToFoldThing[Result[A]] ><> Enumeratee.scanLeft[FoldThing[Result[A]]].apply[(State, Result[B])](f.init -> Empty) { (stateAndResult, next) =>
			val (state, _) = stateAndResult // the _2 is the previously-emitted result, and can be ignored now
			next match {
				// no change to the state for empty values
				case Continue(Empty) => state -> Empty
				// reset the fold to `f.init`, and emit the error
				case Continue(e: Error) => f.init -> e
				// fold the input into the state
				case Continue(Success(input)) => f.fold(state, input)
				case Finish => f.init -> f.finish(state)
			}
		} ><> Enumeratee.map(_._2)
	}

	val entryTransformer = makeScannerThingy(EntryFolder) ><> takeThroughFirstError

	// it works, yay
	// Enumerator(1,2,3,4,5,6) &> upgradeToFoldThing |>>> Iteratee.foreach(println)

	case class FoldEmitState(
		accumKey: Option[String] = None,
		accumValues: List[Int] = Nil,
		emit: Option[Result[Entry]] = None
	)
//	type State = (Option[String], List[Int])
//	type Emit = Option[Entry]
//	Enumeratee.scanLeft[RKV].apply[(State, Emit)](None -> Nil -> None){ (accum, nextResult) =>
//		???
//	}
	type FRKV = FoldThing[RKV]

//	val scanner = Enumeratee.scanLeft[FRKV].apply(FoldEmitState()) { (state, nextResult) =>
//		nextResult match {
//			case Continue(rkv) => rkv match {
//				case Empty => state.copy(emit = None)
//				case e: Error => FoldEmitState(None, Nil, Some(e))
//				case Success(kv) => kv match {
//					case Left(key) =>
//						state.accumKey match {
//							// starting a new entry
//							case None => FoldEmitState(Some(key))
//							// replacing an old entry
//							case Some(oldKey) =>
//								val toEmit = Entry(oldKey, state.accumValues)
//								FoldEmitState(Some(key), Nil, Some(Success(toEmit)))
//						}
//					case Right(value) =>
//						state.accumKey match {
//							// no key to put the values with
//							case None => state.copy(emit = None)
//							// add the value to the accumValues
//							case k =>
//								FoldEmitState(k, state.accumValues :+ value, None)
//						}
//				}
//			}
//			case Finish =>
//				val toEmit = Result.fromOption(state.accumKey).map(key => Entry(key, state.accumValues))
//				FoldEmitState(None, Nil, Some(toEmit))
//		}
//	} ><> Enumeratee.map{ s => println(s" -- intermediate state: $s"); s.emit }

	// ><> Enumeratee.collect{ case FoldEmitState(_, _, Some(emit)) => emit } ><> Enumeratee.takeWhile(!_.isError)

//	case class TakeUntilState(
//		emit: Option[Result[Entry]] = None,
//		numErrorsEmitted: Int = 0,
//		isEmittingError: Boolean = false
//	)

	case class ErrorCountState[A](result: Result[A], numErrorsEmitted: Int = 0)

	/** An Enumeratee that accumulates an error count for resutls passed through it.
		* The `numErrorsEmitted` counter will increment **after** a state with an error
		* result. In this manner, the first error passed through will be accompanied by
		* `numErrorsEmitted = 0`, but the very next event will have `numErrorsEmitted = 1`.
		* This behavior can be used in combination with a `TakeWhile` enumeratee to limit
		* the number of errors accepted by the downstream consumers, or kill the stream
		* at (or just after) an error.
		*
		* @tparam A The type of the results being passed through
		*/
	def foldErrorCounts[A]: Enumeratee[Result[A], ErrorCountState[A]] = {
		Enumeratee.scanLeft[Result[A]](ErrorCountState[A](Empty)){ (state, next) =>
			val incErrorCount = if(state.result.isError) 1 else 0
			ErrorCountState(next, state.numErrorsEmitted + incErrorCount)
		}
	}

	def takeThroughNthError[A](n: Int) = foldErrorCounts[A]
		.compose(Enumeratee.takeWhile(_.numErrorsEmitted < n))
		.compose(Enumeratee.map(_.result))

	def takeThroughFirstError[A] = takeThroughNthError[A](1)

	def takeUntilNthError[A](n: Int) = foldErrorCounts[A]
		.compose(Enumeratee.takeWhile{ s => s.result.isError && s.numErrorsEmitted >= n - 1})
		.compose(Enumeratee.map(_.result))

	def takeUntilFirstError[A] = takeUntilNthError[A](1)

//	def takeUntilNthError[A](n: Int) = foldErrorCounts[A]
//		.compose(Enumeratee.takeWhile{ s => s.numErrorsEmitted <})


//	val floop2 = Enumeratee.scanLeft[Option[Result[Entry]]](TakeUntilState()){ (state, nextEmit) =>
//		val incErrorCount = if(state.isEmittingError) 1 else 0
//		val isError = nextEmit.exists(_.isError)
//		TakeUntilState(nextEmit, state.numErrorsEmitted + incErrorCount, isError)
//	}

//	val combinedFlooper = upgradeToFoldThing ><> scanner ><> floop2
//		.compose(Enumeratee.takeWhile(_.numErrorsEmitted < 1))
//		.compose(Enumeratee.collect{ case TakeUntilState(Some(emit), _, _) => emit })
//		.compose(Enumeratee.map{e => println(s"combined flooper: $e"); e })


//	type RKV = Result[Either[String, Int]]
//	val enumerateVersion2 = Enumeratee.grouped[RKV]{
//		for {
//			_ <- Enumeratee.takeWhile[RKV]{
//				case Success(Left(_)) => false
//				case _ => true
//			} &>> Iteratee.ignore
//
//		}
//	}

	val cleanerParser = (
		(* / "entrylist" / ("key" | "value").extractName).through(keyOrValueParser).transformWith(entryTransformer).mapR{
			entry => println(s" -- entryParser entry: $entry"); entry
		}.parseList &
		(* / "anotherThing" % Text).map{s => println(s" -- anotherThing: $s"); s }
	).tupled

//	val multimapParser = (
//		(* / "entrylist" / ("key" | "value").extractName through keyOrValueParser transformWith combinedFlooper).map{
//			entry => println(s"entryParser entry: $entry"); entry
//		}.parseList &
//		(* / "anotherThing" % Text).map{s => println(s"Got anotherThing: $s"); s}
//	).join{(entryList, anotherThing) =>
//		println(s"entries: $entryList, anotherThing: $anotherThing")
//		"foo"
//	}

	val mainParser = (Root / "multimap").through(cleanerParser) foreachResult println
//	val secondPart = firstPart andThen wrapEnumerateeForResults(enumerateEntries)
//	val secondPart = firstPart transformWith { _ => combinedFlooper }
//		Enumeratee.takeWhile[RKV](!_.isError) ><> Enumeratee.take(1) ><> floop
//		Enumeratee.breakE[RKV](_.isError) ><> floop
//	}

	mainParser parse rawXml
	/*parseWith { implicit ec =>
		type KV = Either[String, Int]
		val getKey = Enumeratee.take(1)

		Enumeratee.grouped[Either[String, Int]].apply[Entry] {
			for {
				key <- Iteratee.head[KV].map(_.get.left.get)
				values <- Enumeratee.takeWhile[KV](_.isRight) ><> Enumeratee.map(_.right.get) &>> Iteratee.getChunks
			} yield {
				Entry(key, values)
			}
		} &>> Iteratee.getChunks
	}*/
}
