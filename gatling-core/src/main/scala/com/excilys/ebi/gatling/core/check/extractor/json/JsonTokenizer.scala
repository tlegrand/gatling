package com.excilys.ebi.gatling.core.check.extractor.json
import scala.annotation.tailrec
import java.util.regex.Pattern
import com.excilys.ebi.gatling.core.util.StringHelper.EMPTY

object JsonTokenizer {

	class JsonTokenizerException(expression: String) extends IllegalArgumentException("Malformed expression: " + expression)

	val SPLIT_PATTERN = Pattern.compile("/")
	val ARRAY_ELEMENT_PATTERN = Pattern.compile("""(.+?)\[(\d+?)\]""")

	def tokenize(string: String): List[JsonPathElement] = {

		@tailrec
		def analyze(strings: List[String], elements: List[JsonPathElement]): List[JsonPathElement] = strings match {
			case Nil => elements
			case string :: others => {
				val element = string match {
					case EMPTY =>
						if (elements == Nil)
							JsonRootWildCard
						else
							throw new JsonTokenizerException(string)
					case "*" => NodeWildCard
					case str =>
						val matcher = ARRAY_ELEMENT_PATTERN.matcher(str)
						if (matcher.find)
							ArrayElementNode(matcher.group(1), matcher.group(2).toInt)
						else
							SimpleNode(str)
				}
				analyze(others, element :: elements)
			}
		}

		if (!string.startsWith("/"))
			throw new JsonTokenizerException(string)

		analyze(SPLIT_PATTERN.split(string.stripPrefix("/"), 0).toList, Nil).reverse
	}

	def main(args: Array[String]) {
		println(tokenize("//BBB"));
		println(tokenize("//DDD/BBB"));
		println(tokenize("/AAA/CCC/DDD/*"));
		println(tokenize("/*/*/*/BBB"));
		println(tokenize("//*"));
		println(tokenize("/AAA/BBB[1]"));
		//		println(tokenize("BBB"));
		//		println(tokenize("/AAA/BBB[last()]"));
	}
}