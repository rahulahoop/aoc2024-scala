object DaySeven extends App {

  private val input = io.Source.fromResource("day_seven.txt").getLines().toSeq

  extension (l: Long)
    def concat(another: Long) = s"$l$another".toLong

  private def operate(lines: Seq[String], withConcat: Boolean) =
    lines.map {
        case s"$total: $rest" => (total.toLong, rest.split(" ").map(_.toLong).toList)
      }
      .filter((total, nums) => go(total, nums.head, nums.tail, withConcat))
      .map((t, _) => t)
      .sum

  def partOne(operations: Seq[String]) = operate(operations, false)

  def partTwo(operations: Seq[String]) = operate(operations, true)

  private def go(result: Long, a: Long, tail: List[Long], includeConcat: Boolean = false): Boolean = {
    tail match
      case Nil => result == a
      case next :: rest =>
        go(result, next + a, rest, includeConcat)
          || go(result, a * next, rest, includeConcat)
          || (if includeConcat then go(result, a.concat(next), rest, includeConcat) else false)
  }

  println(s"result for partOne is ${partOne(input)}")
  println(s"result for partTwo is ${partTwo(input)}")

  val testInput: String =
    """190: 10 19
      |3267: 81 40 27
      |83: 17 5
      |156: 15 6
      |7290: 6 8 6 15
      |161011: 16 10 13
      |192: 17 8 14
      |21037: 9 7 18 13
      |292: 11 6 16 20""".stripMargin

  def testPartOne(): Unit = {
    assert(partOne(testInput.split("\n").toSeq) == 3749)
  }

  def testPartTwo(): Unit = {
    assert(partTwo(testInput.split("\n").toSeq) == 11387)
  }

  testPartOne()
  testPartTwo()
}
