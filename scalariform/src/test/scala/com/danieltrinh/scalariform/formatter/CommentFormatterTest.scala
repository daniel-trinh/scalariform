package scalariform.formatter

import scalariform.parser._
import scalariform.formatter._
import scalariform.formatter.preferences._

// format: OFF
class CommentFormatterTest extends AbstractFormatterTest {

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.scriptBody()
  
  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

  override val debug = false

  """/** 
    |*a
    |b
    | */c""" ==>
  """/**
    | * a
    | * b
    | */
    |c"""

  """/** 
    |*a
    |b
    | */""" ==>
  """/**
    | * a
    | * b
    | */
    |"""

  """/**
    | *
    | *Wibble*/ 
    |class X""" ==>
  """/**
    | *
    | * Wibble
    | */
    |class X"""

  """/***/
    |class A""" ==>
  """/***/
    |class A"""

  """/** */
    |class A""" ==>
  """/** */
    |class A"""

  """/** a */
    |class A""" ==>
  """/** a */
    |class A"""

  """/**
    | * {{
    | *   wibble
    | * }}
    | */
    |class A""" ==>
  """/**
    | * {{
    | *   wibble
    | * }}
    | */
    |class A"""

  """/**
    |*
    |*/""" ==>
  """/**
    | *
    | */
    |"""

  """/** a
    |  * b */""" ==>
  """/**
    | * a
    | * b
    | */
    |"""
      
  // nested comments
  """/**
    |/*
    |*/
    |*/""" ==>
  """/**
    | * /*
    | * */
    | */
    |"""
      
  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(MultilineScaladocCommentsStartOnFirstLine, true)

  """/** This method applies f to each 
    | *  element of the given list.
    | */""" ==>
  """/** This method applies f to each
    | *  element of the given list.
    | */
    |""" 

  """/** Foo
    |Bar
    |*Baz  */""" ==>
  """/** Foo
    | *  Bar
    | *  Baz
    | */
    |"""

  """/** Foo
    |*/""" ==>
  """/** Foo
    | */
    |"""

  """/**
    |*/""" ==>
  """/**
    | */
    |"""
  }
  
  {
  implicit val formattingPreferences = FormattingPreferences.setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)

  """/** This method applies f to each 
    | * element of the given list.
    | */""" ==>
  """/**
    |  * This method applies f to each
    |  * element of the given list.
    |  */
    |""" 

  """/** Foo
    |Bar
    |*Baz  */""" ==>
  """/**
    |  * Foo
    |  * Bar
    |  * Baz
    |  */
    |"""

  """/** Foo
    |*/""" ==>
  """/**
    |  * Foo
    |  */
    |"""

  """/**
    |*/""" ==>
  """/**
    |  */
    |"""
  }
  
  {
  implicit val formattingPreferences = FormattingPreferences
    .setPreference(MultilineScaladocCommentsStartOnFirstLine, true)
    .setPreference(PlaceScaladocAsterisksBeneathSecondAsterisk, true)
  """/** This method applies f to each 
    | * element of the given list.
    | */""" ==>
  """/** This method applies f to each
    |  * element of the given list.
    |  */
    |""" 

  """/** Foo
    |Bar
    |*Baz  */""" ==>
  """/** Foo
    |  * Bar
    |  * Baz
    |  */
    |"""

  """/** Foo
    |*/""" ==>
  """/** Foo
    |  */
    |"""

  """/**
    |*/""" ==>
  """/**
    |  */
    |"""

  """/**          This method applies f to each
    | * element of the given list.
    | */""" ==>
  """/** This method applies f to each
    |  * element of the given list.
    |  */
    |"""
  }

  {
    implicit val formattingPreferences = FormattingPreferences
      .setPreference(RightMargin, 10)

    // This is the example from
    // http://en.wikipedia.org/wiki/Line_wrap_and_word_wrap#Algorithm
    """/**
      | * aaa bb cc ddddd
      | */""" ==>
      """/**
        | * aaa bb
        | * cc
        | * ddddd
        | */
        |"""

    // A long line is broken on spaces.
    """/**
      | * xx xx xx xx xx xx xx xx xx
      | */""" ==>
      """/**
        | * xx xx
        | * xx xx
        | * xx xx
        | * xx xx
        | * xx
        | */
        |"""

    """/**
      | * x x x x x x x x x x
      | *
      | * a a a a a a a a a a
      | *
      | * b b b b b b b b b b
      | */""" ==>
      """/**
        | * x x x x
        | * x x x x
        | * x x
        | *
        | * a a a a
        | * a a a a
        | * a a
        | *
        | * b b b b
        | * b b b b
        | * b b
        | */
        |"""

    // When there is no space before the margin, the first space after the
    // margin is used:
    """/**
      | * This supercalifragilistic comment breaks when possible.
      | */""" ==>
      """/**
        | * This
        | * supercalifragilistic
        | * comment
        | * breaks
        | * when
        | * possible.
        | */
        |"""

    // When there is custom formatting used, it is left alone, so long as
    // it fits within the margin:
    """/**
      | * My list
      | *   - one
      | *   - two
      | * about
      | * stuff.
      | */""" ==>
      """/**
        | * My list
        | *   - one
        | *   - two
        | * about
        | * stuff.
        | */
        |"""

    // Very long last words in paras still work:
    """/**
      | * x x x x xxxxxxxx
      | *
      | * a a a a a a aaaaaaaa
      | */""" ==>
      """/**
        | * x x x x
        | * xxxxxxxx
        | *
        | * a a a a
        | * a a
        | * aaaaaaaa
        | */
        |"""
  }
}
