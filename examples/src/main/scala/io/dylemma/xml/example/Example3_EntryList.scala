package io.dylemma.xml.example

import io.dylemma.spac.old._
import io.dylemma.spac.old.xml._
import javax.xml.stream.events.XMLEvent

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
	Since the `<key>` and `<value>` elements are mixed in no particular order, we'll model
	the contents of `<entrylist>` as a series of `Either[String, Int]`, where `Left` represents
	a `<key>`, and `Right` represents a `<value>`.
	To do so, we'll use a Splitter to capture the element names as "context",
	which will be passed into a function that chooses a parser for an element based on its name.
	 */
	type KeyOrValue = Either[String, Int]
	def keyOrValueParser(elemType: String): XMLParser[KeyOrValue] = elemType match {
		case "key" => XMLParser.forText.map(Left(_))
		case "value" => XMLParser.forText.map(s => Right(s.toInt))
	}
	val keyOrValueTransformer: Transformer[XMLEvent, KeyOrValue] = XMLSplitter("entrylist" \ extractElemName).map(keyOrValueParser)


	/*
	If we simply wanted the list of keys and values, we'd use the `.parseToList` method on our transformer
	to turn it into a Parser that collects all of the data passed through it as a list.

	make a `Splitter` to extract the element name
	in the right context, send it through the `keyOrValueParser`, and use `parseToList` to collect the results.
	 */
	val entryListParserV1 = keyOrValueTransformer.parseToList
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
	val kvToEntryTransformer: Transformer[KeyOrValue, Entry] = Splitter.splitOnMatch[KeyOrValue]{ _.isLeft } map {
		Parser.toList[KeyOrValue].map { keyAndValues =>
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
	val entryListParserV2: XMLParser[List[Entry]] = {
		keyOrValueTransformer.andThen(kvToEntryTransformer).parseToList
	}

	println("V2 results:")
	println(entryListParserV2 parse rawXml)

}
