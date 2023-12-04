import scala.io.Source

def readInput(path: String): Vector[String] =
  Source.fromFile(path).getLines.toVector

def printInput(input: Vector[String]): Unit =
  for (item <- input) {
    println(item)
  }

def helper(s: String): Int =
  val id: Int = s.split(" ")(1).dropRight(1).toInt
  val s2: String = s.dropWhile(c => c != ':').drop(1)
  var result = id

  for (item <- s2.split(";")) {
    for (substr <- item.split(", ")) {
      val num_cubes: Int = substr
        .foldLeft("") { (acc, char) =>
          if (char.isDigit) acc + char else acc
        }
        .toInt

      if (substr.contains("red")) {
        if (num_cubes > 12) {
          result = 0
        }
      } else if (substr.contains("green")) {
        if (num_cubes > 13) {
          result = 0
        }
      } else if (substr.contains("blue")) {
        if (num_cubes > 14) {
          result = 0
        }
      }
    }
  }

  return result

def solve1(inp: Vector[String]): Int =
  inp.map(helper(_)).sum

def helper2(s: String): Int =
  val s2: String = s.dropWhile(c => c != ':').drop(1)
  var max_red: Int = 0
  var max_green: Int = 0
  var max_blue: Int = 0

  for (item <- s2.split(";")) {
    for (substr <- item.split(", ")) {
      val num_cubes: Int = substr
        .foldLeft("") { (acc, char) =>
          if (char.isDigit) acc + char else acc
        }
        .toInt

      if (substr.contains("red")) {
        if (num_cubes > max_red) {
          max_red = num_cubes
        }
      } else if (substr.contains("green")) {
        if (num_cubes > max_green) {
          max_green = num_cubes
        }
      } else if (substr.contains("blue")) {
        if (num_cubes > max_blue) {
          max_blue = num_cubes
        }
      }
    }
  }
  return max_red * max_green * max_blue

def solve2(inp: Vector[String]): Int =
  inp.map(helper2(_)).sum

@main def main(): Unit = {
  val path = "input.txt"
  val input = readInput(path)
  // printInput(input)

  val part1 = solve1(input)
  val part2 = solve2(input)
  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}
