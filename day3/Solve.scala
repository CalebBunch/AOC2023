import scala.io.Source

def readInput(path: String): Vector[String] = {
  Source.fromFile(path).getLines.toVector
}

def printInput(input: Vector[String]): Unit = {
  for (item <- input) {
    println(item)
  }
}

def isNotDigit(ch: Char): Boolean = {
  !ch.isDigit
}

def checkNeighbors(i1: Int, i2: Int, v: Vector[String]): Boolean = {
  val numRows = v.length
  val numCols = if (numRows > 0) v(0).length else 0

  def isValidIndex(row: Int, col: Int): Boolean =
    row >= 0 && row < numRows && col >= 0 && col < numCols

  val neighbors =
    List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

  var res: Boolean = false

  for ((dr, dc) <- neighbors) {
    val adjacentRow = i1 + dr
    val adjacentCol = i2 + dc

    if (
      isValidIndex(adjacentRow, adjacentCol) && isNotDigit(
        v(adjacentRow)(adjacentCol)
      ) && v(adjacentRow)(adjacentCol) != '.'
    ) {
      res = true
    }
  }

  res
}

def solve(input: Vector[String]): Int = {
  var res: Int = 0
  for ((line, i1) <- input.zipWithIndex) {
    var idx: Int = -1
    for (substr <- line.split("(?<=\\D)|(?=\\D)")) {
      if (substr.nonEmpty && substr.forall(_.isDigit)) {
        val int_substr: Int = substr.toInt
        var symbol: Boolean = false

        for (ch <- substr) {
          idx += 1
          symbol = symbol || checkNeighbors(i1, idx, input)
        }
        if (symbol) {
          res += int_substr
        }
      } else {
        idx += substr.length
      }
    }
    idx = 0
  }
  res
}

def checkNeighbors2(i1: Int, i2: Int, v: Vector[String]): Array[(Int, Int)] = {
  val numRows = v.length
  val numCols = if (numRows > 0) v(0).length else 0

  def isValidIndex(row: Int, col: Int): Boolean =
    row >= 0 && row < numRows && col >= 0 && col < numCols

  val neighbors =
    List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))

  var res: Array[(Int, Int)] = Array()

  for ((dr, dc) <- neighbors) {
    val adjacentRow = i1 + dr
    val adjacentCol = i2 + dc

    if (
      isValidIndex(adjacentRow, adjacentCol) && v(adjacentRow)(
        adjacentCol
      ).isDigit
    ) {
      res :+= (adjacentRow, adjacentCol)
    }
  }
  res
}

def constructInts(
    neighbors: Array[(Int, Int)],
    v: Vector[String]
): Array[(Int, Int, Int)] = {
  var result: Array[(Int, Int, Int)] = Array()
  var uniqueIntegers: Set[Int] = Set()

  for ((r, c) <- neighbors) {
    var s: String = ""
    var offset = 0

    while (c + offset < v(r).length && v(r)(c + offset).isDigit) {
      s += v(r)(c + offset)
      offset += 1
    }

    offset = -1
    while (c + offset >= 0 && v(r)(c + offset).isDigit) {
      s = s"${v(r)(c + offset)}$s"
      offset -= 1
    }

    val currentInt = s.toInt

    if (
      !uniqueIntegers.contains(currentInt) || !result
        .exists(tuple => tuple._1 == currentInt && tuple._2 == r)
    ) {
      result :+= ((currentInt, r, c))
      uniqueIntegers += currentInt
    }
  }

  result
}

def solve2(input: Vector[String]): Int = {
  var res: Int = 0
  for ((line, i1) <- input.zipWithIndex) {
    var idx: Int = 0
    val pattern = "(?<=\\*)|(?=\\*)".r
    for (substr <- pattern.split(line)) {
      if (substr == "*") {
        var neighbors: Array[(Int, Int)] = checkNeighbors2(i1, idx, input)
        if (!neighbors.isEmpty) {
          val ints: Array[(Int, Int, Int)] = constructInts(neighbors, input)
          if (ints.length == 2) {
            res += (ints(0)(0) * ints(1)(0))
          }
        }
      }
      idx += substr.length
    }
    idx = 0
  }
  res
}

@main def main(): Unit = {
  val path = "input.txt"
  val input = readInput(path)
  // printInput(input)

  val result = solve2(input)
  println(s"The result is: $result")
}
