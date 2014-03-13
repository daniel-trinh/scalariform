package scalariform.formatter

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import FormatterDirectiveParser.getDirectives

// format: +preserveSpaceBeforeArguments
class FormatterDirectiveParserTest extends FlatSpec with Matchers {

  it should "parse formatter ON/OFF instructions" in {
    "format: ON " ==> ToggleFormatting(true)
    "format: OFF" ==> ToggleFormatting(false)
  }

  it should "parse option toggle instructions" in {
    "format: +rewriteArrowSymbols" ==> ToggleOption(true, "rewriteArrowSymbols")
    "format: -rewriteArrowSymbols" ==> ToggleOption(false, "rewriteArrowSymbols")
    "format: -rewriteArrowSymbols, +spaceBeforeColon" ==> (ToggleOption(false, "rewriteArrowSymbols"), ToggleOption(true, "spaceBeforeColon"))
  }

  it should "parse despite surrounding junk" in {
    "// wibble wobble\nformat: OFF" ==> ToggleFormatting(false)
    "// wibble wobble\nformat: OFF\nwobblewobble" ==> ToggleFormatting(false)
    "// wibble wobble\nformat: OFFwobblewobble" ==> ToggleFormatting(false)
    "blahformat: -rewriteArrowSymbols, +spaceBeforeColon\nblah" ==> (ToggleOption(false, "rewriteArrowSymbols"), ToggleOption(true, "spaceBeforeColon"))
  }

  implicit class String2FormatTest(s: String) extends FormatTest(s.stripMargin)

  case class FormatTest(source: String) {
    def ==>(expectedDirectives: FormatterDirective*) {
      getDirectives(source) should be (expectedDirectives)
    }
  }

}
