package io.dylemma.xml.example

import io.dylemma.xml.Parser.MultiplexedParser
import io.dylemma.xml.ParsingDSL._
import io.dylemma.xml.Result._
import io.dylemma.xml._
import play.api.libs.iteratee.Execution.Implicits.trampoline

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

	/* This folder takes a stream of keys and values, grouping them up into Entries.
	 * Internal state is an Option[Key] + List[Value].
	 */
	object EntryFolder extends StreamScan[Either[String, Int], Entry] {
		type State = (Option[String], List[Int])
		def init = None -> Nil
		def finish(state: State) = {
			Result.fromOption(state._1).map(Entry(_, state._2))
		}
		def fold(state: State, input: Either[String, Int]) = input match {
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

	val keysOrValues = * / "entrylist" / ("key" | "value").extractName through keyOrValueParser
	val entryTransformer = keysOrValues.scanResultsWith(EntryFolder).takeThroughFirstError

	val cleanerParser = (
		entryTransformer.mapR{ entry => println(s" -- entryParser entry: $entry"); entry }.parseList &
		(* / "anotherThing" % Text).map{s => println(s" -- anotherThing: $s"); s }
	).tupled

	val mainParser = (Root / "multimap").through(cleanerParser) foreachResult println

	mainParser parse rawXml
}
