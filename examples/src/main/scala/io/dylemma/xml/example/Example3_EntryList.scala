package io.dylemma.xml.example

import io.dylemma.xsp._

/**
 * Created by dylan on 11/24/2015.
 */
object Example3_EntryList extends App {

	/*
	In this example, we have XML containing a bunch of <key> and <value> elements
	which we want to assemble into a list of `Entry` instances. Each <key> marks
	the beginning of an Entry, and each <value> following a <key> should be added
	to the entry's `values` list.
	 */

	case class Entry(key: String, values: List[Int])

	val rawXml = """<entrylist>
		|  <key>hello</key>
		|  <value>5</value>
		|  <value>2</value>
		|  <key>goodbye</key>
		|  <key>floopy</key>
		|  <value>3</value>
		|  <value>4</value>
		|  <value>5</value>
		|  <key>orange</key>
		|  <value>3</value>
		|  <value>4</value>
		|  <value>5</value>
		|</entrylist>
		|""".stripMargin


	/*
	First we need a parser for individual <key> and <value> elements.
	`Parser.choose` creates a parser that needs context passed in from some other source.
	We'll assume that context will be the name of the element ("key" or "value"), and choose an
	appropriate inner parser based on that name. We'll maep the results to `Either` so the Parser's output type
	doesn't resolve as `Any`; using `Left` for keys, and `Right` for values.
	 */
	type KeyOrValue = Either[String, Int]
	val keyOrValueParser: Parser[String, KeyOrValue] = Parser.choose[String]{
		case "key" => Parser.forText.map(Left(_))
		case "value" => Parser.forText.map(s => Right(s.toInt))
	}

	/*
	If we simply wanted the list of keys and values, we'd make a `Splitter` to extract the element name
	in the right context, send it through the `keyOrValueParser`, and use `parseToList` to collect the results.
	 */
	val entryListParserV1 = Splitter("entrylist" \ extractElemName).through(keyOrValueParser).parseToList
	println("V1 result:")
	println(entryListParserV1 parse rawXml)
	println()

	/*
	But we don't want a list of keys and values, we want a list of *Entries*.
	It would be possible to `map` the `entryListParserV1` to assemble the raw keys and values to entries
	once all of them had been collected, but that would be wasteful of memory, especially in a scenario
	where you might have millions of keys and values.

	What we really want is to define a `Transformer` that takes in keys and values as input, and emits
	Entries as output. `Splitter.splitOnMatch` will be helpful for this purpose.
	`splitOnMatch` divides the input stream into substreams, starting a new substream whenever the 'matcher' function
	finds a match. This way we can get substreams likeSubstream(key, value, value), Substream(key, value, value...).
	Then we'll use the ToList consumer to collect the keys and values from each substream,
	then map the results from that consumer to `Entry` instances.
	 */
	val kvToEntryTransformer: Transformer[KeyOrValue, Entry] = Splitter.splitOnMatch[KeyOrValue]{ _.isLeft }.through {
		Consumer.ToList[KeyOrValue].map { keyAndValues =>
			// we can safely assume that there will be at least one element in the `keyAndValues`
			// list, and that it is a `Left`, due to the `splitOnMatch` condition
			val Left(key) :: valueRights = keyAndValues
			val values = valueRights collect { case Right(value) => value }
			Entry(key, values)
		}
	}

	/*
	Now that the transformer is defined, we can modify the V1 parser.
	Note the difference between V1 and V2 is the addition of `.andThen(kvToEntryTransformer)`
	 */
	val entryListParserV2: Parser[Any, List[Entry]] = {
		Splitter("entrylist" \ extractElemName).through(keyOrValueParser).andThen(kvToEntryTransformer).parseToList
	}

	println("V2 results:")
	println(entryListParserV2 parse rawXml)

}
