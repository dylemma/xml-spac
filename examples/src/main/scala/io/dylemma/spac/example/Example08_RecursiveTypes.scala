package io.dylemma.spac
package example

import cats.syntax.apply._
import io.dylemma.spac.xml._

// based on https://github.com/dylemma/xml-spac/issues/19
object Example08_RecursiveTypes {

	/* Some example type exists, e.g.
	 *     case class Group(id: Int, name: String, subGroups: Stream[Group])
	 * which is represented in XML as:
	 */
	val xmlSource = JavaxSource.fromString {
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
	}

	/* The problem is that (to my knowledge) java.xml.stream doesn't support a "mark/reset"
	 * interaction for an XmlEvent stream. So it would be impossible to actually return a
	 * `Stream[Group]` for each group, since at any given point there would be lots of
	 * potential "stream heads" (due to there being at least one Group in memory for each
	 * point in the Group hierarchy). Supporting all of those group streams would mean that
	 * each time you tried to get the stream's tail, the underlying XmlEvent stream would
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
	implicit val groupContextParser: XmlParser[GroupContext] = (
		Splitter.xml(* \ "id").text.map(_.toInt).parseFirst,
		Splitter.xml(* \ "name").text.parseFirst
	).mapN(GroupContext.apply)

	// given a <group> element, parse a GroupContext, then get a Transformer that can find the subGroups,
	// making sure that the GroupContext we parsed is treated as a "parent" for recursion involving the `stack`
	def groupTransformer(stack: List[GroupContext]): Transformer[XmlEvent, List[GroupContext]] = {
		groupContextParser.followedByStream { context =>
			val nestedContext = context :: stack
			val after = Splitter.xml(* \ "groups" \ "group").flatMap(_ => groupTransformer(nestedContext))
			new SinglePrefixTransformer[XmlEvent, List[GroupContext]](nestedContext, after) through Transformer.map(_.reverse)
		}
	}

	// a Transformer that sends a `prefix` element downstream before acting as a pass-through
	class SinglePrefixTransformer[In, T](prefix: T, transformer: Transformer[In, T]) extends Transformer[In, T] {
		def newHandler = new Transformer.Handler[In, T] {
			private val inner = transformer.newHandler
			private var sentPrefix = false
			def push(in: In, out: Transformer.HandlerWrite[T]) = {
				val s1 =
					if (!sentPrefix) {
						sentPrefix = true
						out.push(prefix)
					}
					else Signal.Continue
				s1 || inner.push(in, out)
			}
			def finish(out: Transformer.HandlerWrite[T]) = {
				if (!sentPrefix) out.push(prefix)
				inner.finish(out)
			}
		}
	}

	def main(args: Array[String]): Unit = {
		xmlSource.iterateWith { xmlEvents =>
			val itr = groupTransformer(Nil).transform(xmlEvents)

			// this will print:
			// List(GroupContext(1, a))
			// List(GroupContext(2, b), GroupContext(1, a))
			for (ctx <- itr) println(ctx)
			/* What to do with this `itr` is left as an exercise for the reader */
		}
	}
}
