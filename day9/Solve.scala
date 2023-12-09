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
  var res: Int = 0
  for (s <- input) {
    var sequence: Array[Int] = s.split(" ").map(_.toInt)
    var sequenceSum: Int = sequence.last
    while (!sequence.forall(_ == 0)) {
      var differences: Array[Int] = Array()
      for (i <- 0 until sequence.length - 1) {
        differences :+= sequence(i + 1) - sequence(i)
      }
      sequenceSum += differences.last
      sequence = differences
    }
    res += sequenceSum
  }
  res
}

def solve2(input: Array[String]): Int = {
  var res: Int = 0
  for (s <- input) {
    var sequence: Array[Int] = s.split(" ").map(_.toInt)
    var sequenceSum: Int = sequence.head
    while (!sequence.forall(_ == 0)) {
      var differences: Array[Int] = Array()
      for (i <- 0 until sequence.length - 1) {
        differences :+= sequence(i) - sequence(i + 1)
      }
      sequenceSum += differences.head
      sequence = differences
    }
    res += sequenceSum
  }
  res
}

@main def main(): Unit = {
  val path = "input.txt"
  val input = readInput(path)
  // printInput(input)

  val part1: Int = solve1(input)
  val part2: Int = solve2(input)
  println(f"Part 1: $part1")
  println(f"Part 2: $part2")
}
