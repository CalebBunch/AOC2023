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

  for (i <- res.indices) {
    for (r <- ranges) {
      if (res(i) >= r._2 && res(i) < r._2 + r._3) {
        res(i) = r._1 + ((r._2 + r._3) - res(i))
      }
    }
  }

  res.min
}

@main def main(): Unit = {
  val path = "input.txt"
  val input = readInput(path)
  println(Long.MaxValue)
  // printInput(input)

  val result = solve1(input)
  println(s"The result is: $result")
}
