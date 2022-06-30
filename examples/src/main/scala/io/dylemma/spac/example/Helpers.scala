package io.dylemma.spac.example


import java.net.URI
import java.nio.file.{FileSystems, Path, Paths}

object Helpers {
	/** Resolves one of the src/main/resources files in this module as a java.nio.file.Path.
	  * Deals with the complication of resources being compiled into JAR files, causing the
	  * `getResource` to return a URI like `file:/path/to/temp/examples.jar!/path/to/resource`
	  * which would cause `Paths.get` to fail normally.
	  */
	def exampleResource(resourcePath: String): Path = {
		val fullUri = this.getClass.getResource(resourcePath).toURI.toString
		val parts = fullUri.split("!").toList
		parts match {
			case one :: Nil =>
				Paths.get(one)
			case outer :: inner :: Nil =>
				val fs = FileSystems.newFileSystem(URI.create(outer), new java.util.HashMap[String, String])
				fs.getPath(inner)
			case _ =>
				throw new IllegalArgumentException(s"Don't know how to resolve $fullUri as a file")
		}
	}
}
