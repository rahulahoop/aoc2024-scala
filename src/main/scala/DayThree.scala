import scala.util.matching.Regex

object DayThree extends App {

  private val input = io.Source.fromResource("day_three.txt").mkString

  val pattern: Regex = """mul\(\d{1,3},\d{1,3}\)""".r

  def partOne(input: String): Int = {
    pattern.findAllIn(input).foldLeft(0) { (acc, next) =>
      val add = next match
        case s"mul($a, $b)" => a.toInt * b.toInt
        case _ => 0
      acc + add
    }
  }

  val adapted: Regex = """(mul\(\d{1,3},\d{1,3}\))|(don't\(\))|(do\(\))""".r

  def partTwo(input: String) = {
    val (result, _) = adapted.findAllIn(input).foldLeft((0, false)) { case ((acc, shouldIgnore), next) =>
      next match
        case "don't()" => (acc, true)
        case "do()" => (acc, false)
        case s"mul($a, $b)" if !shouldIgnore =>
          (acc + (a.toInt * b.toInt), shouldIgnore)
        case _ => (acc, shouldIgnore)
    }

    result
  }

  println(s"the result for partOne is ${partOne(input)}")
  println(s"the result for partTwo is ${partTwo(input)}")

  /* Test */

  val testInput = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

  def testPartOne() = {
    assert(partOne(testInput) == 161)
  }

  val testInputTwo = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

  def testPartTwo() = {
    assert(partTwo(testInputTwo) == 48)
  }

  testPartOne()
  testPartTwo()
}
