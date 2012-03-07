package com.excilys.ebi.gatling.core.check.extractor.json

sealed trait JsonPathElement

case object JsonRootWildCard extends JsonPathElement
case object NodeWildCard extends JsonPathElement
case class SimpleNode(name: String) extends JsonPathElement
case class ArrayElementNode(name: String, index: Int) extends JsonPathElement
