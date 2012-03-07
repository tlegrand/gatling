/**
 * Copyright 2011-2012 eBusiness Information, Groupe Excilys (www.excilys.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * 		http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.excilys.ebi.gatling.core.check.extractor.json
import org.codehaus.jackson.JsonFactory
import com.excilys.ebi.gatling.core.util.IOHelper.use
import com.excilys.ebi.gatling.core.check.extractor.json.JsonExtractor.FACTORY
import scala.collection.immutable.Stack
import org.codehaus.jackson.JsonToken
import org.codehaus.jackson.JsonParser
import scala.annotation.tailrec
import com.excilys.ebi.gatling.core.check.extractor.Extractor.{ toOption, seqToOption }

object JsonExtractor {
	lazy val FACTORY = new JsonFactory
}

class JsonExtractor(textContent: String) {

	@tailrec
	private def walkRec(expectedPath: List[JsonPathElement], parser: JsonParser, stack: Stack[JsonPathElement], depth: Int, results: List[String]): (Stack[JsonPathElement], Int, List[String]) = {
		if (depth == 0)
			(stack, depth, results)
		else {
			val (newStack, newDepth, newResults) = parser.nextToken match {
				case JsonToken.START_ARRAY => (stack.push(ArrayElementNode(parser.getCurrentName, 0)), depth + 1, results)
				case JsonToken.START_OBJECT => {
					val head = if (stack.isEmpty) None else Some(stack.head)
					val newStack = head match {
						case Some(ArrayElementNode(name, index)) => stack.pop.push(ArrayElementNode(name, index + 1))
						case _ => stack.push(SimpleNode(parser.getCurrentName))
					}
					(newStack, depth + 1, results)
				}
				case JsonToken.END_ARRAY => (stack.pop, depth - 1, results)
				case JsonToken.END_OBJECT => {
					val head = if (stack.isEmpty) None else Some(stack.head)
					val newStack = head match {
						case Some(ArrayElementNode(name, index)) => stack
						case Some(_) => stack.pop
						case None => stack
					}
					(newStack, depth - 1, results)
				}
				case JsonToken.FIELD_NAME => (stack, depth, results)
				case _ => {
					val actualPath = SimpleNode(parser.getCurrentName) :: stack.toList
					val unstackedExpectedPath = JsonTokenizer.unstack(expectedPath, actualPath.length)
					val newResults = if (JsonPathMatcher.matchPath(unstackedExpectedPath, actualPath)) parser.getText :: results else results
					(stack, depth, newResults)
				}
			}
			walkRec(expectedPath, parser, newStack, newDepth, newResults)
		}
	}

	def extractOne(occurrence: Int)(expression: String): Option[String] = extractMultiple(expression) match {
		case Some(results) if (results.length > occurrence) => results(occurrence)
		case _ => None
	}

	def extractMultiple(expression: String): Option[Seq[String]] = use(FACTORY.createJsonParser(textContent)) { parser =>
		val expected = JsonTokenizer.tokenize(expression)
		parser.nextToken
		val (_, _, results) = walkRec(expected, parser, Stack[JsonPathElement](), 1, Nil)
		results.reverse
	}

	def count(expression: String): Option[Int] = extractMultiple(expression).map(_.size)
}