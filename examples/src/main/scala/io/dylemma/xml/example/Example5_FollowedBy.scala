package io.dylemma.xml.example

import javax.xml.stream.events.XMLEvent

import io.dylemma.spac._
import io.dylemma.spac.xml._

object Example5_FollowedBy extends App {
	val xml ="""<doc>
		| <users>
		|  <user id="1">dylemma</user>
		|  <user id="2">someone else</user>
		| </users>
		| <message user_id="1">Hello</message>
		| <message user_id="2">'sup?</message>
		| <message user_id="1">cool new library features</message>
		|</doc>""".stripMargin

	/*
	Sometimes you encounter a situation like the above, where you have some "dictionary" element (`<users>`)
	followed by a bunch of elements which reference that dictionary. If you want to create a `Transformer`
	for the `<message>` elements where the user name is filled in, you can use `followedByStream` to pass
	the parsed `<users>` map in, so you don't have to collect all of the messages to a List (which would be
	memory intensive for longer lists).
	 */

	case class User(id: String, name: String)
	case class UserMap(map: Map[String, User]) extends (String => User) {
		def apply(userId: String) = map(userId)
	}
	case class Message(user: User, content: String)

	// parser for a <user> element to a `User`
	implicit val userParser: XMLParser[User] = (
		XMLParser.forMandatoryAttribute("id") and
		XMLParser.forText
	).as(User)

	// parser for a <users> element to a `UserMap`
	implicit val userMapParser: XMLParser[UserMap] = XMLSplitter(* \ "user").as[User].parseToList.map { userList =>
		UserMap(userList.map(u => u.id -> u).toMap)
	}

	// parser to get the first <users> element from the document
	val usersParser = XMLSplitter(* \ "users").first[UserMap]

	// parser for <message> elements, given a function to get a User by its id
	def getMessageParser(userMap: UserMap): XMLParser[Message] = (
		XMLParser.forMandatoryAttribute("user_id").map(userMap) and
		XMLParser.forText
	).as(Message)

	/*
	The result of `usersParser` can now be used to create a transformer that will receive all
	<message> elements encountered after the <users> have been parsed.
	 */
	val messagesTransformer: Transformer[XMLEvent, Message] = usersParser.followedByStream { userMap =>
		XMLSplitter(* \ "message").as(getMessageParser(userMap))
	}

	// a Transformer that passes events through after printing them
	val eventPrinter = Transformer.sideEffect[XMLEvent](e => println(s"  xml event [$e]"))

	// Here I'm adding a println for each XMLEvent to show exactly when each message is parsed during the stream:
	// the important point is that they are parsed *during* the stream, rather than at the end
	println("messagesTransformer:")
	(eventPrinter >> messagesTransformer).parseForeach(println).parse(xml)
	println("---\n")

	/*
	You can also use for-comprehension syntax with `followedByStream`, as `followedByStream`
	is actually an object with identical `apply` and `flatMap` methods
	 */
	val messagesTransformer2: Transformer[XMLEvent, Message] = for {
		userMap <- usersParser.followedByStream
		message <- XMLSplitter(* \ "message").as(getMessageParser(userMap))
	} yield message
	println("messagesTransformer2:")
	messagesTransformer2.parseForeach(println).parse(xml)
	println("---\n")

	/*
	The for-comprehension syntax makes it easier to combine your "dictionary" info with your "data"
	inside the `yield` block instead of creating a Dictionary=>Transformer function ahead of time
	 */
	case class RawMessage(userId: String, content: String)
	implicit val rawMessageParser = (
		XMLParser.forMandatoryAttribute("user_id") and
		XMLParser.forText
	).as(RawMessage)

	val messagesTransformer3: Transformer[XMLEvent, Message] = for {
		userMap <- usersParser.followedByStream
		RawMessage(userId, content) <- XMLSplitter(* \ "message").as[RawMessage]
	} yield Message(userMap(userId), content)

	println("messagesTransformer3:")
	messagesTransformer3.parseForeach(println).parse(xml)
	println("---\n")
}
