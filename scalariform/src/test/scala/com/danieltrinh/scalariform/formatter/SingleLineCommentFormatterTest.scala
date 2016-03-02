package com.danieltrinh.scalariform.formatter

import scalariform.formatter._
import scalariform.formatter.preferences._
import scalariform.parser._

// format: OFF
class SingleLineCommentFormatterTest extends AbstractFormatterTest {

  type Result = CompilationUnit

  def parse(parser: ScalaParser) = parser.scriptBody()

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState())

  override val debug = false

  // Indentation:
  """{
    |  // a
    |// b
    |    //c
    |}
    |""" ==>
  """{
    |  // a
    |  // b
    |  //c
    |}
    |"""

  // Reflow tests:

  {
    implicit val formattingPreferences = FormattingPreferences
      .setPreference(RightMargin, 10)

    // This is the example from
    // http://en.wikipedia.org/wiki/Line_wrap_and_word_wrap#Algorithm
    """// aaa bb cc ddddd
      |""" ==>
      """// aaa bb
        |// cc
        |// ddddd
        |"""

    // A long line is broken on spaces.
    """// xx xx xx xx xx xx xx xx xx
      |""" ==>
      """// xx xx
        |// xx xx
        |// xx xx
        |// xx xx
        |// xx
        |"""

    """// x x x x x x x x x x
      |//
      |// a a a a a a a a a a
      |//
      |// b b b b b b b b b b
      |""" ==>
      """// x x x
        |// x x x
        |// x x x
        |// x
        |//
        |// a a a
        |// a a a
        |// a a a
        |// a
        |//
        |// b b b
        |// b b b
        |// b b b
        |// b
        |"""

    // When there is no space before the margin, the first space after the
    // margin is used:
    """// This supercalifragilistic comment breaks when possible.
      |""" ==>
      """// This
        |// supercalifragilistic
        |// comment
        |// breaks
        |// when
        |// possible.
        |"""

    // When there is custom formatting used, it is left alone, so long as
    // it fits within the margin:
    """// A list
      |//  - one
      |//  - two
      |// about
      |// stuff.
      |""" ==>
      """// A list
        |//  - one
        |//  - two
        |// about
        |// stuff.
        |"""

    // Very long last words in paras still work:
    """// x x x x xxxxxxxx
      |//
      |// a a a a a a aaaaaaaa
      |""" ==>
      """// x x x
        |// x
        |// xxxxxxxx
        |//
        |// a a a
        |// a a a
        |// aaaaaaaa
        |"""


    // Check for off-by-one errors. The margin is "10".
    // The generally accepted meaning of a "n char" max line length includes
    // the \n char, i.e. in a 80 char wide text, the length of the printable
    // string is <= 79 chars.
    """// 4 6 89
      |justRight
      |
      |// 4 6 8 0
      |overLong
      |
      |// 4 6 8
      |okShort
      |
      |{
      |  // 6 89
      |  justRight
      |
      |  // 6 8 0
      |  overLong
      |
      |  // 6 8
      |  okShort
      |
      |  {
      |    // 89
      |    justRight
      |
      |    // 8 0
      |    overLong
      |
      |    // 8
      |    okShort
      |  }
      |}
    """ ==>
      """// 4 6 89
        |justRight
        |
        |// 4 6 8
        |// 0
        |overLong
        |
        |// 4 6 8
        |okShort
        |
        |{
        |  // 6 89
        |  justRight
        |
        |  // 6 8
        |  // 0
        |  overLong
        |
        |  // 6 8
        |  okShort
        |
        |  {
        |    // 89
        |    justRight
        |
        |    // 8
        |    // 0
        |    overLong
        |
        |    // 8
        |    okShort
        |  }
        |}
        |"""
  }
}
