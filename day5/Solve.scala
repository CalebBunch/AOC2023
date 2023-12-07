import scala.io.Source

def readInput(path: String): Array[String] = {
  Source.fromFile(path).getLines.toArray
}

def printInput(input: Array[String]): Unit = {
  for (item <- input) {
    println(item)
  }
}

def solve1(input: Array[String]): Long = {
  var res: Array[Long] = Array()
  var ranges: Array[(Long, Long, Long)] = Array()

  for (s <- input(0).split(" ").tail) {
    res :+= s.toLong
  }

  for (e <- input.slice(3, input.length).filter(_.nonEmpty)) {
    if (e.contains("-")) {
      for (i <- res.indices) {
        var j: Int = 0
        while (j < ranges.length) {
          if (res(i) >= ranges(j)._2 && res(i) < ranges(j)._2 + ranges(j)._3) {
            res(i) += ranges(j)._1 - ranges(j)._2
            j = ranges.length
          }
          j += 1
        }
      }
      ranges = Array.empty[(Long, Long, Long)]

    } else if (e(0).isDigit) {
      var eValues: Array[String] = e.split(" ")
      ranges :+= (eValues(0).toLong, eValues(1).toLong, eValues(2).toLong)
    }
  }

  res.min
}

def solve2(input: Array[String]): Long = {
  var res: Long = Long.MaxValue
  var seedRanges: Array[(Long, Long)] = Array()
  var ranges: Array[(Long, Long, Long)] = Array()

  var si: Int = 0
  var seeds = input(0).split(" ").tail
  while (si < seeds.length - 1) {
    seedRanges :+= (seeds(si).toLong, seeds(si + 1).toLong)
    si += 2
  }
  for (e <- input.slice(3, input.length).filter(_.nonEmpty)) {
    if (e.contains("-")) {
      ranges :+= (0L, 0L, 0L)
    } else if (e(0).isDigit) {
      var eValues: Array[String] = e.split(" ")
      ranges :+= (eValues(0).toLong, eValues(1).toLong, eValues(2).toLong)
    }
  }

  for (seedRange <- seedRanges) {
    println(s"Working on Range: $seedRange")
    for (currentSeed <- seedRange._1 until seedRange._1 + seedRange._2) {
      var current: Long = currentSeed
      var i: Int = 0
      var seen: Boolean = false

      while (i < ranges.length) {
        if (ranges(i) == (0L, 0L, 0L)) {
          seen = false
        } else if (
          current >= ranges(i)._2 && current < ranges(i)._2 + ranges(
            i
          )._3 && !seen
        ) {
          current += ranges(i)._1 - ranges(i)._2
          seen = true
          if (current < 0) {
            println(current)
          }
        }
        i += 1
      }

      if (current < res) {
        res = current
      }
    }
  }
  res
}

@main def main(): Unit = {
  val path = "input.txt"
  val input = readInput(path)
  // printInput(input)

<<<<<<< HEAD
  val part1: Long = solve1(input)
  val part2: Long = solve2(input)
  // Part 2 uses a naive bruteforce approach
  // and runs for 11 minutes on my machine
  println(f"Part 1: $part1")
  println(f"Part 2: $part2")
=======
  val result = solve1(input)
  println(s"The result is: $result")
>>>>>>> 895f5ddef5ffaf32f981802b0662f9f5e19bc5fd
}
