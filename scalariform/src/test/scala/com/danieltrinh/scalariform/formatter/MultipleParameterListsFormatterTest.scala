package com.danieltrinh.scalariform.formatter

import com.danieltrinh.scalariform.parser.{CompilationUnit, ScalaParser}

class MultipleParameterListsFormatterTest extends AbstractExpressionFormatterTest {

  override  def debug = false

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
      |"""

  // See issue #73
  """def mergeMapsCombiningValueMaps[A, B, C](collisionFunc: (C, C) => C)(m1: Map[A, Map[Seq[B], C]], m2: Map[A, Map[Seq[B], C]]): Map[A, Map[Seq[B], C]] = {
    |  mergeMaps(m1, m2)((m11, m22) => mergeMaps(m11, m22)(collisionFunc))
    |}""" ==>
    """def mergeMapsCombiningValueMaps[A, B, C](collisionFunc: (C, C) => C)
      |                                        (m1: Map[A, Map[Seq[B], C]], m2: Map[A, Map[Seq[B], C]]): Map[A, Map[Seq[B], C]] = {
      |  mergeMaps(m1, m2)((m11, m22) => mergeMaps(m11, m22)(collisionFunc))
      |}"""



}
