//> using file "inputs.scala"
//> using dep org.typelevel::cats-parse::1.1.0

package day3
import utils.readFile
import cats._
import cats.data._
import cats.syntax.all._
import cats.parse.Parser
import cats.parse.Rfc5234.digit

@main def day3 =
  val data = readFile("day3.txt").get
  val mulParser = Parser.string("mul").void
  val leftBracketParser = Parser.char('(').void
  val rightBracketParser = Parser.char(')').void
  val commaParser = Parser.char(',').void
  val digitParser =
    digit
      .repAs[String](1, 3)
      .map(_.toInt)
      .repSep(1, commaParser)
      .map(_.reduce(_ * _))

  val validParser =
    mulParser *> leftBracketParser *> digitParser <* rightBracketParser
  val untilValidInput = Parser.anyChar.repUntil(validParser.backtrack).void

  val multiplicationParser = validParser.surroundedBy(untilValidInput.?).rep

  val numbers = multiplicationParser
    .parseAll(data)
    .map(_.sumAll)
  println(numbers)
