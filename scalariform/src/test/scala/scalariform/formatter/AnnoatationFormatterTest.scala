package scalariform.formatter

import scalariform.parser.{CompilationUnit, ScalaParser}
import scalariform.formatter.preferences.{ForceAnnotationToNextLine, IndentPackageBlocks, FormattingPreferences}

/**
 * Created by bambucha on 02.03.14.
 */
class AnnoatationFormatterTest extends AbstractFormatterTest {

  override def debug: Boolean = true

  def parse(parser: ScalaParser) = parser.compilationUnitOrScript()

  type Result = CompilationUnit

  def format(formatter: ScalaFormatter, result: Result) = formatter.format(result)(FormatterState(indentLevel = 0))

  "def asdf(@annotation one: Int, @a @b(c) two: String)" ==> "def asdf(@annotation one: Int, @a @b(c) two: String)"


  {

    implicit val formattingPreferences = FormattingPreferences.setPreference(ForceAnnotationToNextLine, true)

    """class X {
      |  def asdf(
      |    @annotation one: Int,
      |    @a @b(c) two: String
      |    ) = ???
      |}""".stripMargin ==>
    """
      |class X {
      |  def asdf(
      |    @annotation
      |    one: Int,
      |    @a
      |    @b(c)
      |    two: String
      |  )
      |}""".stripMargin

    """class A extends B {
      |  @SomeImportantAnnotation(param = true) override val param: Int = 1
      |
      |  @NotSoImportantAnnotation(param = false) def description: String = "Not so important"
      |}""".stripMargin ==>
    """class A extends B {
      |  @SomeImportantAnnotation(param = true)
      |  override val param: Int = 1
      |
      |  @NotSoImportantAnnotation(param = false)
      |  def description: String = "Not so important"
      |}""".stripMargin

  }
}
