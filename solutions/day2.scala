//> using file "inputs.scala"
import utils.readFile

def isSafe(line: Vector[Int]): Boolean =
  val res = line
    .sliding(2)
    .map(pair => (pair(1) - pair(0), pair(0) - pair(1)))
    .foldLeft((true, true)):
      case ((allInc, allDec), (inc, dec)) =>
        (allInc && inc >= 1 && inc <= 3, allDec && dec >= 1 && dec <= 3)
  res._1 || res._2

def exercise2(line: Vector[Int]): Boolean =
  if isSafe(line) then true
  else
    (0 until line.length).exists: i =>
      isSafe(line.patch(i, Nil, 1))

@main def day2 =
  val data = utils
    .readFile("day2.txt")
    .get
    .split("\n")
    .map(_.split(" ").map(_.toInt).to(Vector))

  val ex1 = data
    .map(isSafe)
    .foldLeft(0)((acc, cond) => if cond then acc + 1 else acc)

  val ex2 = data
    .map(exercise2)
    .foldLeft(0)((acc, cond) => if cond then acc + 1 else acc)

  println(s"Result: $ex1")
  println(s"Result: $ex2")
