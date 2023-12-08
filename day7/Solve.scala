import scala.io.Source

def readInput(path: String): Array[String] = {
  Source.fromFile(path).getLines.toArray
}

def printInput(input: Array[String]): Unit = {
  for (item <- input) {
    println(item)
  }
}

def getHandStrength1(key: String): Int = {
  val counts = key.distinct.map(ch => key.count(_ == ch))
  if (counts.contains(5)) 0
  else if (counts.contains(4)) 1
  else if (counts.contains(3) && counts.contains(2)) 2
  else if (counts.contains(3)) 3
  else if (counts.count(_ == 2) == 2) 4
  else if (counts.contains(2)) 5
  else 6
}

def compareHands1(hand1: String, hand2: String): Int = {
  val characterStrengths: String = "AKQJT98765432"
  val strengthComparison = getHandStrength1(hand1) - getHandStrength1(hand2)
  if (strengthComparison != 0) strengthComparison
  else
    hand1
      .zip(hand2)
      .map { case (c1, c2) =>
        characterStrengths.indexOf(c1) - characterStrengths.indexOf(c2)
      }
      .find(_ != 0)
      .getOrElse(0)
}

def solve1(input: Array[String]): Int = {
  var res: Int = 0

  var inputMap: Map[String, Int] = input.map { hand =>
    val Array(key, value) = hand.split(" ")
    key -> value.toInt
  }.toMap

  val sortedHands = inputMap.keys.toList.sortWith(compareHands1(_, _) < 0)
  var scoreMultiplier: Int = input.length
  for (hand <- sortedHands) {
    res += scoreMultiplier * inputMap(hand)
    scoreMultiplier -= 1
  }
  res
}

def getHandStrength2(key: String): Int = {
  val counts = key.distinct.filter(_ != 'J').map(ch => key.count(_ == ch))
  val jokers: Int = key.count(_ == 'J')
  if (jokers == 5) 0
  else if (counts.max + jokers == 5) 0
  else if (counts.max + jokers == 4) 1
  else if ((counts.sorted.reverse(0) + jokers + counts.sorted.reverse(1)) == 5)
    2
  else if (counts.max + jokers == 3) 3
  else if ((counts.sorted.reverse(0) + jokers + counts.sorted.reverse(1)) == 4)
    4
  else if (counts.max + jokers == 2) 5
  else 6
}

def compareHands2(hand1: String, hand2: String): Int = {
  val characterStrengths: String = "AKQT98765432J"
  val strengthComparison = getHandStrength2(hand1) - getHandStrength2(hand2)
  if (strengthComparison != 0) strengthComparison
  else
    hand1
      .zip(hand2)
      .map { case (c1, c2) =>
        characterStrengths.indexOf(c1) - characterStrengths.indexOf(c2)
      }
      .find(_ != 0)
      .getOrElse(0)
}

def solve2(input: Array[String]): Int = {
  var res: Int = 0

  var inputMap: Map[String, Int] = input.map { hand =>
    val Array(key, value) = hand.split(" ")
    key -> value.toInt
  }.toMap

  val sortedHands = inputMap.keys.toList.sortWith(compareHands2(_, _) < 0)
  var scoreMultiplier: Int = input.length
  for (hand <- sortedHands) {
    res += scoreMultiplier * inputMap(hand)
    scoreMultiplier -= 1
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
