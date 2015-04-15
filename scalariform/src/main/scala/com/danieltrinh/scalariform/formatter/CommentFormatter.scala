package scalariform.formatter

import scala.collection.mutable.ListBuffer
import scalariform.parser._
import scalariform.utils._
import scalariform.lexer._
import scalariform.formatter.preferences._
import scala.annotation.tailrec

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

  def formatComment(comment: HiddenToken, indentLevel: Int): String =
    if (comment.rawText contains '\n') {
      val sb = new StringBuilder
      val (start, rawLines) = getLines(comment.rawText)

      val alignBeneathSecondAsterisk = formattingPreferences(PlaceScaladocAsterisksBeneathSecondAsterisk)
      val startOnFirstLine = formattingPreferences(MultilineScaladocCommentsStartOnFirstLine)
      val beforeStarSpaces = if (alignBeneathSecondAsterisk) "  " else " "
      val afterStarSpaces = if (startOnFirstLine && !alignBeneathSecondAsterisk) "  " else " "

      val lines = reflowIfNecessary(
        pruneEmptyFinal(pruneEmptyInitial(rawLines)),
        effectiveIndentLevel = indentLevel + beforeStarSpaces.length + 1 + afterStarSpaces.length)

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
   * Reflows the text in the given lines to fit within 'rightMargin', if any lines
   * currently extend beyond that margin.
   *
   * @param effectiveIndentLevel The number of chars that will appear before the text
   *                             in each line.
   */
  private def reflowIfNecessary(lines: List[String], effectiveIndentLevel: Int): List[String] = {
    val effectiveMargin = formattingPreferences(RightMargin) - effectiveIndentLevel

    if (!formattingPreferences(ReflowComments) ||
      !lines.exists(_.length > effectiveMargin)) {

      lines
    } else {
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
            while (currentInput.length > effectiveMargin) {
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
  }

  /**
   * Find the index of the last space char before the margin, if one exists.
   * Else, find the index of the first space char after the margin, if one
   * exists.
   * Else, return -1
   */
  private def findLineBreakPoint(line: String, margin: Int): Int = {
    line.lastIndexOf(' ', margin) match {
      case -1 =>
        line.indexOf(' ', margin)
      case x => x
    }
  }
}
