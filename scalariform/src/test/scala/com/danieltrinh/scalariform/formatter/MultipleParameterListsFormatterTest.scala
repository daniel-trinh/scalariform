package com.danieltrinh.scalariform.formatter

import com.danieltrinh.scalariform.formatter.preferences.{BreakMultipleParameterGroups, FormattingPreferences}
import com.danieltrinh.scalariform.parser.{CompilationUnit, FullDefOrDcl, ScalaParser}

class MultipleParameterListsFormatterTest extends AbstractFormatterTest {

  override  def debug = false

  def parse(parser: ScalaParser) = parser.compilationUnitOrScript()

  type Result = CompilationUnit

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))


  {

    implicit val formatting = FormattingPreferences.setPreference(BreakMultipleParameterGroups, true)

     """def f(x: Int)
   |(y: Int): Int = {
    |}
    |""".stripMargin ==>
 """def f(x: Int)(y: Int): Int = {
    |}
    |""".stripMargin
     """def f(x: Int)
    |     (y: Int)(z: Int): Int = {
    |}
    |""".stripMargin ==>
 """def f(x: Int)(y: Int)(z: Int): Int = {
    |}
    |""".stripMargin

  }

  {

    implicit val formatting = FormattingPreferences.setPreference(BreakMultipleParameterGroups, true)
    .setPreference(BreakMultipleParameterGroups.BreakingThreshold, 4)

    """def f(x: Int)(y: Int): Int = {
    |}
    |""".stripMargin ==>
      """def f(x: Int)
      |     (y: Int): Int = {
      |}
      |""".stripMargin

    """def f(x: Int)
    |     (y: Int)(z: Int): Int = {
    |}
  """.stripMargin ==>
    """def f(x: Int)
      |     (y: Int)
      |     (z: Int): Int = {
      |}
      |""".stripMargin

   // See issue #73
    """def mergeMapsCombiningValueMaps[A, B, C](collisionFunc: (C, C) => C)(m1: Map[A, Map[Seq[B], C]], m2: Map[A, Map[Seq[B], C]]): Map[A, Map[Seq[B], C]] = {
    |  mergeMaps(m1, m2)((m11, m22) => mergeMaps(m11, m22)(collisionFunc))
    |}""".stripMargin ==>
      """def mergeMapsCombiningValueMaps[A, B, C](collisionFunc: (C, C) => C)
      |                                        (m1: Map[A, Map[Seq[B], C]], m2: Map[A, Map[Seq[B], C]]): Map[A, Map[Seq[B], C]] = {
      |  mergeMaps(m1, m2)((m11, m22) => mergeMaps(m11, m22)(collisionFunc))
      |}""".stripMargin

     }



}
