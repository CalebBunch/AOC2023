import scala.io.Source

def readInput(path: String): Array[String] = {
  Source.fromFile(path).getLines.toArray
}

def printInput(input: Array[String]): Unit = {
  for (item <- input) {
    println(item)
  }
}

def solve1(input: Array[String]): Int = {
  val times: Array[Int] =
    input(0).split(" ").tail.filter(_.nonEmpty).map(_.toInt)
  val distances: Array[Int] =
    input(1).split(" ").tail.filter(_.nonEmpty).map(_.toInt)

  times
    .zip(distances)
    .map { case (time, distance) =>
      (1 until time).count(holdTime => holdTime * (time - holdTime) > distance)
    }
    .product
}

def solve2(input: Array[String]): Int = {
  val time: Long =
    input(0).split(" ").tail.filter(_.nonEmpty).mkString.toLong
  val distance: Long =
    input(1).split(" ").tail.filter(_.nonEmpty).mkString.toLong

  (1L until time).count(holdTime => holdTime * (time - holdTime) > distance)
}

@main def main(): Unit = {
  val path = "input.txt"
  val input = readInput(path)
  // printInput(input)

  val part1: Int = solve1(input)
  val part2: Long = solve2(input)
  println(f"Part 1: $part1")
  println(f"Part 2: $part2")
}
