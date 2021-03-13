package io.dylemma.xml.example

import io.dylemma.spac.old._
import io.dylemma.spac.old.xml._
import javax.xml.stream.events.XMLEvent

// based on https://github.com/dylemma/xml-spac/issues/19
object Example8_RecursiveTypes {

	/* Some example type exists, e.g.
	 *     case class Group(id: Int, name: String, subGroups: Stream[Group])
	 * which is represented in XML as:
	 */
	val xml =
		"""<group>
		  |  <id>1</id>
		  |  <name>a</name>
		  |  <groups>
		  |    <group>
		  |      <id>2</id>
		  |      <name>b</name>
		  |    </group>
		  |  </groups>
		  |</group>
		""".stripMargin

	/* The problem is that (to my knowledge) java.xml.stream doesn't support a "mark/reset"
	 * interaction for an XMLEvent stream. So it would be impossible to actually return a
	 * `Stream[Group]` for each group, since at any given point there would be lots of
	 * potential "stream heads" (due to there being at least one Group in memory for each
	 * point in the Group hierarchy). Supporting all of those group streams would mean that
	 * each time you tried to get the stream's tail, the underlying XMLEvent stream would
	 * have to reset to the location of that particular subGroup stream, and resume parsing.
	 *
	 * So since that's impossible, we try something else:
	 *
	 * Flatten the `subGroups` concept so you have only one overall stream.
	 * To do so, create a `GroupContext` class that mirrors `Group`, minus the `subGroups` field.
	 * Then create a parser/transformer that creates stacks of GroupContexts to represent
	 * each group.
	 * For the top-level group, that would be a stack with one element:
	 *     GroupContext(1, "a") :: Nil
	 * For its child groups, they would be a stack where the first group context is at the bottom (end of the list),
	 * and the respective child would be at the top (head of the list), e.g.
	 *     GroupContext(2, "b") :: GroupContext(1, "a") :: Nil
	 * Each time we encounter a new group, it would be represented as a stack where its non-streaming
	 * info is at the top of the stack
	 */

	// our modified "Group" model
	case class GroupContext(id: Int, name: String)

	// given a <group> element, parse a GroupContext
	implicit val groupContextParser: XMLParser[GroupContext] = (
		XMLSplitter(* \ "id").asText.map(_.toInt).parseFirst and
		XMLSplitter(* \ "name").asText.parseFirst
	).as(GroupContext)

	// given a <group> element, parse a GroupContext, then get a Transformer that can find the subGroups,
	// making sure that the GroupContext we parsed is treated as a "parent" for recursion involving the `stack`
	def groupTransformer(stack: List[GroupContext]): Transformer[XMLEvent, List[GroupContext]] = {
		groupContextParser.followedByStream { context =>
			val nestedContext = context :: stack
			val after = XMLSplitter(* \ "groups" \ "group").flatMap(groupTransformer(nestedContext))
			new SinglePrefixTransformer[XMLEvent, List[GroupContext]](nestedContext, after) >> Transformer.map(_.reverse)
		}
	}

	// a Transformer that sends a `prefix` element downstream before acting as a pass-through
	class SinglePrefixTransformer[In, T](prefix: T, transformer: Transformer[In, T]) extends Transformer[In, T] {
		def makeHandler[Out](next: Handler[T, Out]): Handler[In, Out] = new Handler[In, Out] {
			val tNext = transformer.makeHandler(next)
			var didPrepend = false
			def isFinished: Boolean = didPrepend && next.isFinished
			def handlePrefix(): Option[Out] = {
				if(!didPrepend) {
					didPrepend = true
					next.handleInput(prefix)
				} else {
					None
				}
			}
			def handleInput(input: In): Option[Out] = {
				handlePrefix() orElse tNext.handleInput(input)
			}
			def handleError(error: Throwable): Option[Out] = {
				handlePrefix() orElse tNext.handleError(error)
			}
			def handleEnd(): Out = {
				handlePrefix() getOrElse tNext.handleEnd()
			}
		}
	}

	def main(args: Array[String]): Unit = {
		// this will print:
		// List(GroupContext(1, a))
		// List(GroupContext(2, b), GroupContext(1, a))
		for(ctx <- groupTransformer(Nil).transform(xml)) println(ctx)

		/* What you do with this stream is left as an exercise for the reader */
	}
}
