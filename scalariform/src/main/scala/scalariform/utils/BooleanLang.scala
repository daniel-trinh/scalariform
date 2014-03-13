package scalariform.utils

object BooleanLang {

  def not(b: Boolean) = !b

  implicit class PimpedBoolean(b1: Boolean) {
    def and(b2: Boolean) = b1 && b2
    def or(b2: Boolean) = b1 || b2
  }
}
