package io.dylemma

/** SPaC (short for "<strong>S</strong>treaming <strong>Pa</strong>rser <strong>C</strong>ombinators")
  * is a library for building stream consumers in a declarative style, specialized for tree-like data
  * types like XML and JSON.
  *
  * Many utilities for handling XML and JSON data involve parsing the entire "document" to some DOM model,
  * then inspecting and transforming that model to extract information.
  * The downside to these utilities is that when the document is very large, the DOM may not fit in memory.
  * The workaround for this type of problem is to treat the document as a stream of "events",
  * e.g. "StartElement" and "EndElement" for XML, or "StartObject" and "EndObject" for JSON.
  * The downside to this workaround is that writing code to handle these streams can be complicated and
  * error-prone, especially when the DOM is complicated.
  *
  * SPaC's goal is to drastically simplify the process of creating code to handle these streams.
  *
  * This package contains the "core" SPaC traits; `Parser`, `Transformer`, `Splitter`, and `ContextMatcher`.
  *
  * See the `xml` and `json` subpackages (provided by the `xml-spac` and `json-spac` libraries respectively)
  * for specific utilities related to handling XML and JSON event streams.
  *
  * @groupname primary Main Concepts
  * @groupname context Capturing Context Data
  * @groupname errors Error Handling
  * @groupname util Utility and Supporting Classes
  * @groupprio primary 0
  * @groupprio context 2
  * @groupprio errors 1
  * @groupprio util 3
  * @groupdesc primary All event consumers in SPaC are defined in terms of `Parser`, `Splitter`, and `Transformer`.
  *            Each of these three classes are interrelated, but with the eventual goal of producing one or more
  *            interpreted values given an incoming stream of event data.
  * @groupdesc context When dealing with tree-like documents, it is often important to be able to express a relative
  *            location in that data, or to produce some value based on the current location within the tree.
  *            SPaC refers to these locations as "context".
  * @groupdesc errors SPaC parsers should only ever throw `SpacException` from their `parse` and `parseF` methods.
  *            SpacException is a specialized exception type which uses "Spac Trace" elements instead of the usual
  *            "Stack Trace"; these provide more useful information like what part of the parser failed, some
  *            contextual information about what event caused the parser to fail.
  * @groupdesc util Most of these classes and traits are typeclasses that the primary types operate in terms of.
  *            Generally you don't directly interact with these.
  */
package object spac
