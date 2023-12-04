import scala.io.Source

def readInput(path: String): Vector[String] = {
  Source.fromFile(path).getLines.toVector
}

def printInput(input: Vector[String]): Unit = {
  for (item <- input) {
    println(item)
  }
}

def solve(input: Vector[String]): Int = {
  input
    .map(line => {
      val blocks = line.split("\\s+").filter(_.nonEmpty)
      blocks.length - blocks.distinct.length
    })
    .filter(_ > 0)
    .map(winning_nums => 1 << (winning_nums - 1))
    .sum
}

def isValidIndex(row: Int, numRows: Int): Boolean =
  row >= 0 && row < numRows

def search(v: Vector[String], index: Int, total: Int): (Int, Int) = {
  if (!isValidIndex(index, v.length)) {
    (0, total)
  } else {
    val s = v(index).split("\\s+").filter(_.nonEmpty)
    val copies = s.length - s.distinct.length

    val (_, updatedTotal) = (index + 1 until index + copies + 1)
      .foldLeft((0, total)) { case ((_, accTotal), i) =>
        search(v, i, accTotal)
      }

    (0, updatedTotal + 1)
  }
}

def solve2(input: Vector[String]): Int = {
  input.zipWithIndex.foldLeft(0) { case (acc, (line, index)) =>
    acc + search(input, index, 0)._2
  }
}

@main def main(): Unit = {
  val path = "input.txt"
  val input = readInput(path)
  // printInput(input)

  val result = solve2(input)
  println(s"The result is: $result")
}

// ----------------//////////---------------- //

/*
Part 1: Original Solution (Too Procedural)

def solve(input: Vector[String]): Int = {
  var res: Int = 0
  for (line <- input) {
    val blocks: Array[String] = line.split("\\s+").filter(_.nonEmpty)
    val winning_nums = blocks.length - blocks.toSet.size
    if (winning_nums > 0) {
      res += (1 << (winning_nums - 1))
    }
  }
  res
}
 */

// ----------------//////////---------------- //
