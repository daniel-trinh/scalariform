package scalariform.lexer

abstract sealed class HiddenToken(val token: Token) {

  lazy val newlineful = token.text contains '\n'

  def text = token.text

  def rawText = token.rawText

}

case class Whitespace(override val token: Token) extends HiddenToken(token)

sealed abstract class Comment(token: Token) extends HiddenToken(token)

object Comment {

  def unapply(comment: Comment) = Some(comment.token)

}

/**
 * A comment starting with "//" that extends to the end of the line.
 */
case class SingleLineComment(override val token: Token) extends Comment(token)

/**
 * A comment starting with "/*" that extends until a matching "*/"
 */
case class MultiLineComment(override val token: Token) extends Comment(token)

/**
 * A comment starting "/**" that extends until a matching "*/"
 */
case class ScalaDocComment(override val token: Token) extends Comment(token)
