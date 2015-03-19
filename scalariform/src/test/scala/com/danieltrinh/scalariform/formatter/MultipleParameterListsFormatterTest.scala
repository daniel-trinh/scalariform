package com.danieltrinh.scalariform.formatter

import com.danieltrinh.scalariform.formatter.preferences.{BreakMultipleParameterGroups, FormattingPreferences}

class MultipleParameterListsFormatterTest extends AbstractExpressionFormatterTest {

  override  def debug = false

  {

    implicit val formatting = FormattingPreferences.setPreference(BreakMultipleParameterGroups, true)

     """def f(x: Int)
   |(y: Int): Int = {
    |}
    |""" ==> """def f(x: Int)(y: Int): Int = {
    |}
    |"""
     """def f(x: Int)
    |     (y: Int)(z: Int): Int = {
    |}""" ==>"""def f(x: Int)(y: Int)(z: Int): Int = {
      |}
      |"""

  }

  {

    implicit val formatting = FormattingPreferences.setPreference(BreakMultipleParameterGroups, true)
    .setPreference(BreakMultipleParameterGroups.BreakingThreshold, 4)

    """def f(x: Int)(y: Int): Int = {
    |}
    |""" ==>
      """def f(x: Int)
      |     (y: Int): Int = {
      |}
      |"""

    """def f(x: Int)
    |     (y: Int)(z: Int): Int = {
    |}
  """ ==>
      """def f(x: Int)
      |     (y: Int)
      |     (z: Int): Int = {
      |}
      |""" // See issue #73
    """def mergeMapsCombiningValueMaps[A, B, C](collisionFunc: (C, C) => C)(m1: Map[A, Map[Seq[B], C]], m2: Map[A, Map[Seq[B], C]]): Map[A, Map[Seq[B], C]] = {
    |  mergeMaps(m1, m2)((m11, m22) => mergeMaps(m11, m22)(collisionFunc))
    |}""" ==>
      """def mergeMapsCombiningValueMaps[A, B, C](collisionFunc: (C, C) => C)
      |                                        (m1: Map[A, Map[Seq[B], C]], m2: Map[A, Map[Seq[B], C]]): Map[A, Map[Seq[B], C]] = {
      |  mergeMaps(m1, m2)((m11, m22) => mergeMaps(m11, m22)(collisionFunc))
      |}"""

     }



}
