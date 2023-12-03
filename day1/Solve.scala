import scala.io.Source
import scala.util.control.Breaks._

def readInput(path: String): Vector[String] =
  Source.fromFile(path).getLines.toVector

def printInput(input: Vector[String]): Unit =
  for (item <- input) {
    println(item)
  }

def getDigits(s: String): String =
  val d1 = s.indexWhere(c => c.isDigit)
  val d2 = s.lastIndexWhere(c => c.isDigit)
  s(d1).toString + s(d2).toString

def solve1(inp: Vector[String]): Int =
  val digits = inp.map(i => getDigits(i))
  val res = digits.map(d => d.toInt)
  res.sum

def findWordRight(s: String): (Int, Int) = {
  var result: (Int, Int) = (0, -1)

  breakable {
    for (i <- (s.length - 3) to 0 by -1) {
      val substr = s.substring(i, i + 3)
      substr match {
        case "one" =>
          result = (1, i)
          break
        case "two" =>
          result = (2, i)
          break
        case "thr" =>
          result = (3, i)
          break
        case "fou" =>
          result = (4, i)
          break
        case "fiv" =>
          result = (5, i)
          break
        case "six" =>
          result = (6, i)
          break
        case "sev" =>
          result = (7, i)
          break
        case "eig" =>
          result = (8, i)
          break
        case "nin" =>
          result = (9, i)
          break
        case _ =>
      }
    }
  }
  result
}

def findWordLeft(s: String): (Int, Int) = {
  var result: (Int, Int) = (0, -1)

  breakable {
    s.sliding(3).foreach { substr =>
      substr match {
        case "one" =>
          result = (1, s.indexOf("one"))
          break
        case "two" =>
          result = (2, s.indexOf("two"))
          break
        case "thr" =>
          result = (3, s.indexOf("three"))
          break
        case "fou" =>
          result = (4, s.indexOf("four"))
          break
        case "fiv" =>
          result = (5, s.indexOf("five"))
          break
        case "six" =>
          result = (6, s.indexOf("six"))
          break
        case "sev" =>
          result = (7, s.indexOf("seven"))
          break
        case "eig" =>
          result = (8, s.indexOf("eight"))
          break
        case "nin" =>
          result = (9, s.indexOf("nine"))
          break
        case _ =>
      }
    }
  }
  result
}

def getDigits2(s: String): String =
  val d1 = s.indexWhere(c => c.isDigit)
  val d2 = s.lastIndexWhere(c => c.isDigit)
  val s1 = findWordLeft(s)
  val s2 = findWordRight(s)
  var minLeft: Int = 0
  var minRight: Int = 0

  if (d1 < s1._2 || s1._2 == -1) {
    minLeft = s(d1).asDigit
  } else if (s1._2 < d1 || d1 == -1) {
    minLeft = s1._1
  }

  if (d2 > s2._2 || s2._2 == -1) {
    minRight = s(d2).asDigit
  } else if (s2._2 > d2 || d2 == -1) {
    minRight = s2._1
  }

  minLeft.toString + minRight.toString

def solve2(inp: Vector[String]): Int =
  val digits = inp.map(i => getDigits2(i))
  val res = digits.map(d => d.toInt)
  res.sum

@main def main(): Unit = {
  val path = "input.txt"
  val input = readInput(path)
  // printInput(input)

  val result = solve2(input)
  println(s"The result is: $result")
}
