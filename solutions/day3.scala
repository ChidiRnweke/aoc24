//> using file "inputs.scala"
//> using dep org.typelevel::cats-parse::1.1.0

package day3
import utils.readFile
import cats._
import cats.data._
import cats.syntax.all._
import cats.parse.Parser
import cats.parse.Rfc5234.digit

def exercise1(data: String): Int =
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
  val invalidInput = Parser.anyChar.repUntil(validParser.backtrack).void

  val multiplicationParser = validParser.surroundedBy(invalidInput.?).rep

  multiplicationParser
    .parseAll(data)
    .map(_.sumAll)
    .getOrElse(throw new Exception("Parsing failed"))

def filterDonts(data: String): String =
  val dontParser = Parser.string("don't()").void
  val doParser = Parser.string("do()").void
  val discardDont =
    (dontParser ~ Parser.anyChar.repUntil(doParser.backtrack)).void
  Parser.anyChar
    .surroundedBy(discardDont.?)
    .repAs[String]
    .parseAll(data)
    .getOrElse(throw new Exception("Parsing failed"))

@main def day3 =
  val data = readFile("day3.txt").get
  val numbers = exercise1(data)
  println(numbers)
  val filtered = filterDonts(data)
  val ex2 = exercise1(filtered)
  println(ex2)
