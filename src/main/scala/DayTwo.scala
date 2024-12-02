object DayTwo extends App {

  enum Direction:
    case INC, DEC, NONE

  private val input = io.Source.fromResource("day_two.txt").mkString.split("\n").toSeq

  private def analyze(a: Int, b: Int): (Direction, Boolean) = {
    val direction = if a > b then Direction.DEC else if a == b then Direction.NONE else Direction.INC
    val isValidDistance = Math.abs(a - b) < 4
    (direction, isValidDistance)
  }

  def partOne(reports: Seq[String]): Int = {
    reports
      .map(report =>
        val analysis = report.split(" ")
          .toSeq
          .sliding(2, 1)
          .map(pair => analyze(pair.head.toInt, pair.tail.head.toInt))
          .toSet

        analysis.size == 1
      )
      .count(p => p)
  }

  def partTwo(reports: Seq[String]): Int = {
    reports
      .map(report =>
        val nums = report.split(" ").toSeq

        val analysis = nums
          .sliding(2, 1)
          .map(pair => analyze(pair.head.toInt, pair.tail.head.toInt))
          .toSeq

        if analysis.toSet.size == 1 then true
        else {
          val items = nums.zipWithIndex
          nums.indices.exists(rm =>
            val redoMinusOne = items.filter(a => a._2 != rm)
              .map(_._1)
              .sliding(2, 1)
              .map(pair => analyze(pair.head.toInt, pair.tail.head.toInt))
              .toSeq

            redoMinusOne.toSet.toSeq match
              case (dir, true) :: Nil if dir != Direction.NONE => true
              case _ => false
          )
        }
      ).count(p => p)
  }

  println(s"results for part 1 ist ${partOne(input)}")
  println(s"results for part 2 ist ${partTwo(input)}")

  /* Tests */

  val rawTestInput =
    s"""7 6 4 2 1
       |1 2 7 8 9
       |9 7 6 2 1
       |1 3 2 4 5
       |8 6 4 4 1
       |1 3 6 7 9
       |""".stripMargin


  def testPartOne() = {
    val input = rawTestInput.split("\n").toSeq
    assert(partOne(input) == 2)
  }

  def testPartTwo() = {
    val input = rawTestInput.split("\n").toSeq
    val result = partTwo(input)
    assert(result == 4)
  }

  testPartOne()
  testPartTwo()
}
