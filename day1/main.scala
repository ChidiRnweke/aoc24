//> using resourceDir "day1"
//> using dep "co.fs2::fs2-core:3.11.0"
//> using dep "co.fs2::fs2-io:3.11.0"
//> using dep "org.typelevel::cats-effect::3.5.7"

import fs2.io.file.{Files, Path}
import cats.effect.*
import cats.instances.list

def splitLists: fs2.Stream[IO, (List[Int], List[Int])] =
  val path = Path("day1/input.txt")

  Files[IO]
    .readAll(path)
    .through(fs2.text.utf8.decode)
    .through(fs2.text.lines)
    .fold((List.empty[Int], List.empty[Int]))((acc, line) =>
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

object Main extends IOApp.Simple:
  val run: IO[Unit] =
    splitLists
      .evalTap: lists =>
        val result1 = exercise1(lists._1, lists._2)
        val result2 = exercise2(lists._1, lists._2)
        IO.println(s"Result 1: $result1") >>
          IO.println(s"Result 2: $result2")
      .compile
      .drain
