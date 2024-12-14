//> using file inputs.scala

import utils.readFile

def splitLists(): (List[Int], List[Int]) =
  readFile("day1.txt").get
    .split("\n")
    .foldLeft((List.empty[Int], List.empty[Int]))((acc, line) =>
      val split = line.split("   ")
      (acc._1 :+ split(0).toInt, acc._2 :+ split(1).toInt)
    )

def exercise1(leftList: List[Int], rightList: List[Int]): Int =
  (leftList.sorted zip rightList.sorted)
    .map((pair) => (pair._1 - pair._2).abs)
    .reduce(_ + _)

def exercise2(leftList: List[Int], rightList: List[Int]): Int =
  (leftList zip leftList.map((l) => rightList.count(_ == l)))
    .map((pair) => pair._1 * pair._2)
    .reduce(_ + _)

@main def day1 =
  val (list1, list2) = splitLists()
  val result1 = exercise1(list1, list2)
  val result2 = exercise2(list1, list2)
  println(s"Result 1: $result1")
  println(s"Result 2: $result2")
