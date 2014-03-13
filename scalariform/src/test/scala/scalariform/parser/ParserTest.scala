package scalariform.parser

import scalariform.lexer._

import org.scalatest.FlatSpec
import org.scalatest.Matchers

// format: +preserveSpaceBeforeArguments
class ParserTest extends FlatSpec with Matchers {

  "Parser" should "throw a parse exception" in {
    an[ScalaParserException] should be thrownBy { parseExpression("for {x <- b if }") }
  }

  "Parser" should "throw a parse exception for empty match " in {
    an[ScalaParserException] should be thrownBy  { parseExpression("a match { }") }
  }

  "Parser" should "produce a parse exception on a trailing close brace" in {
    an[ScalaParserException] should be thrownBy  { parseCompilationUnit("class A{}}") }
  }

  "Parser" should "not throw an exception" in {
    parseExpression("{ case List[String]() => 12 }")
  }

  // See issue #60
  "Parser" should "not throw an exception on case block ending with decl" in {
    parseExpression("""
                args(0) match {
                        case "blah" =>
                                val x = args(0)
                        case _ =>
                                println("not blah")
                }
    """)
  }

  "Parser" should "throw a parse exception in bad package blocks" in {
    an[ScalaParserException] should be thrownBy  { parseCompilationUnit("package a {} package b {}") }
  }

  private def parser(s: String) = new ScalaParser(ScalaLexer.tokenise(s).toArray)
  private def parseExpression(s: String) = parser(s).expr()
  private def parseCompilationUnit(s: String) = parser(s).compilationUnit()

}
