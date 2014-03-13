package scalariform.formatter

import scalariform.formatter.preferences._

// format: OFF
class RewriteArrowsTest extends AbstractExpressionFormatterTest {

  {
    implicit val formattingPreferences = FormattingPreferences.setPreference(RewriteArrowSymbols, true)

    "(a: Int) => 3" ==> "(a: Int) ⇒ 3"
    "for (i <- 1 to 10) yield i" ==> "for (i ← 1 to 10) yield i"
  }

  override val debug = false
 
}
