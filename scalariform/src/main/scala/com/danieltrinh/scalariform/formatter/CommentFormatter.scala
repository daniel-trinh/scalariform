package scalariform.formatter

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scalariform.formatter.preferences._
import scalariform.lexer._

trait CommentFormatter { self: HasFormattingPreferences with ScalaFormatter ⇒

  private def getLines(comment: String): (String, List[String]) = {
    val prefix = List("/** ", "/**", "/* ", "/*").find(comment.startsWith).get
    val (start, rest) = comment.splitAt(prefix.length)
    val (contents, _) = rest.splitAt(rest.length - "*/".length)
    val firstLine :: otherLines = contents.split("""\r?\n([ \t]*(\*(?!/))?)?""", Integer.MAX_VALUE).toList
    val afterStarSpaces = if (formattingPreferences(MultilineScaladocCommentsStartOnFirstLine)) 2 else 1
    val initialSpaces = firstLine takeWhile (_.isWhitespace)
    val adjustedLines = dropInitialSpaces(firstLine, initialSpaces.size) :: (otherLines map { dropInitialSpaces(_, afterStarSpaces) })
    //    val adjustedLines map { line ⇒ if (line startsWith "*/") "*" + line else line }
    (start, adjustedLines)
  }

  @tailrec
  private def dropInitialSpaces(s: String, maxSpacesToDrop: Int): String =
    if (maxSpacesToDrop > 0 && s.startsWith(" "))
      dropInitialSpaces(s drop 1, maxSpacesToDrop - 1)
    else
      s

  private def removeTrailingWhitespace(s: String) = s.reverse.dropWhile(_.isWhitespace).reverse

  private def pruneEmptyInitial(lines: List[String]) = lines match {
    case first :: rest if first.trim == "" ⇒ rest
    case _                                 ⇒ lines
  }

  private def pruneEmptyFinal(lines: List[String]) = pruneEmptyInitial(lines.reverse).reverse

  /**
   * Reformats a ScalaDocComment, fixing the star alignment, and reflowing long
   * lines if necessary.
   *
   * A ScalaDocComment of any length appears as a single token, and any newlines
   * and indentation spaces will be represented inside the token string.
   *
   * @return the string representation of the formatted token
   */
  def formatComment(comment: ScalaDocComment, indentLevel: Int): String =
    if (comment.rawText contains '\n') {
      val sb = new StringBuilder
      val (start, rawLines) = getLines(comment.rawText)

      val alignBeneathSecondAsterisk = formattingPreferences(PlaceScaladocAsterisksBeneathSecondAsterisk)
      val startOnFirstLine = formattingPreferences(MultilineScaladocCommentsStartOnFirstLine)
      val beforeStarSpaces = if (alignBeneathSecondAsterisk) "  " else " "
      val afterStarSpaces = if (startOnFirstLine && !alignBeneathSecondAsterisk) "  " else " "

      val effectiveIndentCharWidth =
        indentCharWidth(indentLevel) + beforeStarSpaces.length + 1 + afterStarSpaces.length
      val effectiveMargin = formattingPreferences(RightMargin) - effectiveIndentCharWidth
      val lines = reflowIfNecessary(
        pruneEmptyFinal(pruneEmptyInitial(rawLines)),
        effectiveMargin)

      sb.append(start.trim)

      var firstLine = true
      for (line ← lines) {
        val trimmedLine = removeTrailingWhitespace(line)
        if (firstLine && startOnFirstLine) {
          if (trimmedLine.nonEmpty)
            sb.append(" ").append(trimmedLine)
        } else {
          sb.append(newlineSequence).indent(indentLevel).append(beforeStarSpaces).append("*")
          if (trimmedLine.nonEmpty)
            sb.append(afterStarSpaces).append(trimmedLine)
        }
        firstLine = false
      }
      sb.append(newlineSequence).indent(indentLevel).append(beforeStarSpaces).append("*/")
      sb.toString
    } else
      comment.rawText

  /**
   * Reformats a contiguous block of SingleLineComments, reflowing long lines if necessary.
   *
   * A block of SingleLineComments appears as a list of tokens.
   * The newlines at the end of each comment line are contained within each token
   * string.
   * Any indentation spaces are not contained within the token string, and will
   * be added later. The indentLevel is passed here only to enable the effective
   * margin size to be computed.
   */
  def formatComment(indentLevel: Int)(comment: List[SingleLineComment]): List[SingleLineComment] = {
    val effectiveMargin = formattingPreferences(RightMargin) - indentCharWidth(indentLevel) - "// ".length

    def getTextLength(c: SingleLineComment): Int =
      c.token.rawText.length - "// ".length - newlineSequence.length

    def unwrapSingleLineComment(c: SingleLineComment): String =
      c.token.rawText.replaceFirst("\\A//\\s*(.*?)\\s*\\Z", "$1")

    // We need to create a Token for the modified SingleLineComment.
    // This is only used in a very limited scope, so we don't need to invent real
    // values for all the fields.
    def wrapSingleLineComment(line: String): SingleLineComment =
      SingleLineComment(Token(
        Tokens.LINE_COMMENT,
        "## not used! ##", // It is a bug if this string appears in output!
        0,
        if (line.isEmpty) {
          "//" + newlineSequence
        } else {
          "// " + line + newlineSequence
        }))

    if (!formattingPreferences(ReflowComments) ||
      !comment.exists(getTextLength(_) >= effectiveMargin)) {

      comment
    } else {
      reflow(comment.map(unwrapSingleLineComment), effectiveMargin)
        .map(wrapSingleLineComment)
    }
  }

  /**
   * Reflows the text in the given lines to fit within 'rightMargin', if any lines
   * currently extend beyond that margin.
   *
   * @param effectiveMargin The max allowed length, in chars, of each line.
   */
  private def reflowIfNecessary(lines: List[String], effectiveMargin: Int): List[String] = {
    if (!formattingPreferences(ReflowComments) ||
      !lines.exists(_.length >= effectiveMargin)) {

      lines
    } else {
      reflow(lines, effectiveMargin)
    }
  }

  /**
   * Reflows the text in the given lines to fit within 'rightMargin'.
   *
   * @param effectiveMargin The max allowed length, in chars, of each line.
   */
  private def reflow(lines: List[String], effectiveMargin: Int): List[String] = {
    // TODO: maybe use a TeX style LMS reflow algorithm, instead of this greedy algo?
    // http://en.wikipedia.org/wiki/Line_wrap_and_word_wrap#Algorithm
    // http://www.geeksforgeeks.org/dynamic-programming-set-18-word-wrap/
    // http://ideone.com/e.js/0TTMUn

    // Act per-paragraph. Paragraphs are blank-line delimited.
    val (remainder, reflownLines) = lines.foldLeft("", ListBuffer[String]()) {
      case ((remainderOfPrevLine, acc), nextLine) =>
        if (nextLine.isEmpty) {
          // A blank line means we have finished a paragraph. Any text still
          // held in `remainderOfPrevLine` should be appended as there will be
          // no more text in this paragraph with which to fill up that line.
          if (remainderOfPrevLine.isEmpty) {
            ("", acc :+ nextLine)
          } else {
            ("", acc :+ remainderOfPrevLine :+ nextLine)
          }
        } else {
          // A non-blank line means we are starting or continuing a paragraph.
          // Gather the text in-hand into `currentLine`:
          var currentInput = if (remainderOfPrevLine.isEmpty) {
            nextLine
          } else {
            remainderOfPrevLine + " " + nextLine
          }
          var currentOuput = acc

          // We need a "while" here, just in case the `currentLine` is more than
          // twice as long as the effective margin.
          while (currentInput.length >= effectiveMargin) {
            val breakPoint = findLineBreakPoint(currentInput, effectiveMargin)
            if (breakPoint == -1) {
              // There was nowhere to break the line before the margin; just
              // break at the first point; there's nothing to d
              currentOuput += currentInput
              currentInput = ""
            } else {
              currentOuput += currentInput.substring(0, breakPoint).trim
              currentInput = currentInput.substring(breakPoint + 1).trim
            }
          }

          (currentInput, currentOuput)
        }
    }
    if (remainder.isEmpty) {
      reflownLines.toList
    } else {
      (reflownLines :+ remainder).toList
    }
  }

  /**
   * Find the index of the last space char before the margin, if one exists.
   * Else, find the index of the first space char after the margin, if one
   * exists.
   * Else, return -1
   *
   * Note that traditionally the line margin includes a "\n" char, so we are
   * looking for a space whose index is less than the margin, so that the
   * resulting line length is less than the margin.
   */
  private def findLineBreakPoint(line: String, margin: Int): Int = {
    line.lastIndexOf(' ', margin - 1) match {
      case -1 =>
        line.indexOf(' ', margin)
      case x => x
    }
  }

  /**
   * Return the width, in chars, of the given indent level.
   *
   * The "tab width" is assumed to be 4 (there is not currently a setting for
   * that).
   */
  private def indentCharWidth(indentLevel: Int): Int =
    formattingPreferences.indentStyle.indent(indentLevel).map {
      case '\t' => 4
      case _ => 1
    }.sum
}
