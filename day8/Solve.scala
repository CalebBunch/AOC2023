import scala.io.Source

def readInput(path: String): Array[String] = {
  Source.fromFile(path).getLines.toArray
}

def printInput(input: Array[String]): Unit = {
  for (item <- input) {
    println(item)
  }
}

def search1(inp: Array[String], t: String): Int = {
  var index: Int = -1
  for (i <- inp.indices) {
    if (inp(i).split(" ").head == t) {
      index = i
    }
  }
  index
}

def solve1(input: Array[String]): Int = {
  var res: Int = 0
  var directions: String = input.head
  var current: String = ""

  for (line <- input) {
    if (line.split(" ").head == "AAA") {
      current = line
    }
  }

  var i: Int = 0
  while (current.split(" ").head != "ZZZ") {

    if (i == directions.length) {
      i = 0
    }

    if (directions(i) == 'R') {
      var target: String = current.slice(12, 15)
      current = input(search1(input, target))
    } else {
      var target: String = current.slice(7, 10)
      current = input(search1(input, target))
    }

    i += 1
    res += 1
  }

  res
}

def gcd(a: Long, b: Long): Long = {
  if (b == 0) a
  else gcd(b, a % b)
}

def lcm(numbers: Long*): Long = {
  if (numbers.isEmpty) 0
  else numbers.reduce((a, b) => math.abs(a * b) / gcd(a, b))
}

def search2(inp: Array[String], t: String): Int = {
  inp.drop(2).indexWhere(s => s.slice(0, 3) == t) + 2
}

def solve2(input: Array[String]): Long = {
  var res: Long = 0
  var directions: String = input.head
  var cycles: Array[Long] = Array()
  var alines: Array[String] = Array()
  var current: String = ""

  for (line <- input.slice(2, input.length)) {
    if (line.split(" ").head(2) == 'A') {
      alines :+= line
    }
  }

  for (value <- alines) {
    current = value

    var i: Int = 0
    while (current.split(" ")(0)(2) != 'Z') {

      if (i == directions.length) {
        i = 0
      }

      if (directions(i) == 'R') {
        var target: String = current.slice(12, 15)
        current = input(search1(input, target))
      } else {
        var target: String = current.slice(7, 10)
        current = input(search1(input, target))
      }

      i += 1
      res += 1
    }
    cycles :+= res
    res = 0
  }

  lcm(cycles: _*)
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
