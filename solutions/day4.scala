//> using file "inputs.scala"
//> using dep "org.typelevel::cats-core:2.12.0"

package day4
import cats._
import cats.data._
import cats.syntax.all._

enum MoveDirection:
  case Down, Right, RightUp, RightDown

import utils.readFile

def readData() =
  readFile("day4.txt").get
    .split("\n")
    .map(_.toCharArray().toList)
    .toList

def evalMatrix(matrix: List[List[Char]]): Int =
  val nRows = matrix.length
  val nCols = matrix(0).length
  val values = matrix.mapWithIndex: (rows, rowIdx) =>
    rows.mapWithIndex: (_, colIdx) =>
      evalPosition(matrix, rowIdx, colIdx, nRows, nCols)
  values.map(_.sumAll).sumAll

def evalPosition(
    matrix: List[List[Char]],
    row: Int,
    col: Int,
    numRows: Int,
    numCols: Int
) =
  val d = evalMove(matrix, row, col, MoveDirection.Down, numRows, numCols)
  val r = evalMove(matrix, row, col, MoveDirection.Right, numRows, numCols)
  val ru = evalMove(matrix, row, col, MoveDirection.RightUp, numRows, numCols)
  val rd = evalMove(matrix, row, col, MoveDirection.RightDown, numRows, numCols)
  d + r + ru + rd

def evalMove(
    matrix: List[List[Char]],
    row: Int,
    col: Int,
    move: MoveDirection,
    numRows: Int,
    numCols: Int
): Int =
  move match
    case MoveDirection.Down if row + 4 <= numRows =>
      val sequence = (row until row + 4).map(matrix(_)(col)).toList
      foundXMAS(sequence)
    case MoveDirection.Right if col + 4 <= numCols =>
      val sequence = (col until col + 4).map(matrix(row)(_)).toList
      foundXMAS(sequence)
    case MoveDirection.RightDown if col + 4 <= numCols && row + 4 <= numRows =>
      val sequence = (0 until 4).map(i => matrix(row + i)(col + i)).toList
      foundXMAS(sequence)
    case MoveDirection.RightUp if col + 4 <= numCols && row - 3 >= 0 =>
      val sequence = (0 until 4).map(i => matrix(row - i)(col + i)).toList
      foundXMAS(sequence)
    case _ =>
      0

def foundXMAS(sequence: List[Char]): Int =
  sequence match
    case 'X' :: 'M' :: 'A' :: 'S' :: Nil => 1
    case 'S' :: 'A' :: 'M' :: 'X' :: Nil => 1
    case _                               => 0

@main def day4 =
  val testData = """MMMSXXMASM
                    |MSAMXMSMSA
                    |AMXSXMAAMM
                    |MSAMASMSMX
                    |XMASAMXAMM
                    |XXAMMXXAMA
                    |SMSMSASXSS
                    |SAXAMASAAA
                    |MAMMMXMMMM
                    |MXMXAXMASX""".stripMargin
    .split("\n")
    .map(_.toCharArray().toList)
    .toList

  val ex1 = evalMatrix(readData())
  println(ex1)
