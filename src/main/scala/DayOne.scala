object DayOne extends App {

  private val input = io.Source.fromResource("day_one.txt").mkString.split("\n").toSeq
  private def getLeftAndRight: (Seq[Int], Seq[Int]) =
    (input.map(s => s.split(" {3}").head.toInt), input.map(s => s.split(" {3}").tail.head.toInt))

  def partOne(distancePairs: Seq[String]): Int = {
    val (left, right) = getLeftAndRight
    left.sorted.zip(right.sorted).map((l, r) => Math.abs(l - r)).sum
  }

  def partTwo(distancePairs: Seq[String]): Int = {
    val (left, right) = getLeftAndRight
    val fMap = right.foldLeft(Map.empty[Int, Int]) { (fMap, next) =>
      fMap ++ Map(next -> (fMap.getOrElse(next, 0) + 1))
    }

    left.map(num => num * fMap.getOrElse(num, 0)).sum
  }

  println(s"result for part 1 is ${partOne(input)}")
  println(s"result for part 2 is ${partTwo(input)}")

  /* Tests */

  val rawTestInput: String =
    s"""3   4
       |4   3
       |2   5
       |1   3
       |3   9
       |3   3
       |""".stripMargin

  def testPartOne(): Unit = {
    val testInput: Seq[String] = rawTestInput.split("\n").toSeq
    assert(partOne(testInput) == 11)
  }

  def testPartTwo(): Unit = {
    val testInput: Seq[String] = rawTestInput.split("\n").toSeq
    assert(partTwo(testInput) == 31)
  }

  testPartOne()
  testPartTwo()
}
