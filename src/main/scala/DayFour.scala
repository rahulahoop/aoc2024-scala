import scala.collection.immutable.{AbstractSeq, LinearSeq}

object DayFour extends App {

  private val input = io.Source.fromResource("day_four.txt").mkString.split("\n").toSeq
  private val LINEAR = Seq(
    (1, 0),
    (2, 0),
    (3, 0)
  )

  private val VERTICAL = Seq(
    (0, 1),
    (0, 2),
    (0, 3)
  )

  private val DIAGNOL = Seq(
    (1, 1),
    (2, 2),
    (3, 3),
  )

  private def buildCoörds(puzzle: Seq[String]) = {
    val searchMap: Seq[Seq[Char]] = puzzle.map(_.toCharArray.toSeq)
    for {
      y <- puzzle.indices
      x <- 0 until puzzle(y).length
    } yield (x, y) -> searchMap(y)(x)
  }

  def partOne(puzzle: Seq[String]): Int = {
    val coords = buildCoörds(puzzle)
    val puzzleMap: Map[(Int, Int), Char] = coords.toMap

    coords.flatMap((pos, c) => {
      if c == 'X' then
        Seq(
          LINEAR.flatMap((x, y) => puzzleMap.get(pos._1 + x, pos._2 + y)), // straight
          LINEAR.flatMap((x, y) => puzzleMap.get(pos._1 - x, pos._2 + y)), // backwards
          DIAGNOL.flatMap((x, y) => puzzleMap.get(pos._1 + x, pos._2 + y)), // down right
          DIAGNOL.flatMap((x, y) => puzzleMap.get(pos._1 + x, pos._2 - y)), // up right
          DIAGNOL.flatMap((x, y) => puzzleMap.get(pos._1 - x, pos._2 + y)), // down left
          DIAGNOL.flatMap((x, y) => puzzleMap.get(pos._1 - x, pos._2 - y)), // up left
          VERTICAL.flatMap((x, y) => puzzleMap.get(pos._1 + x, pos._2 + y)), // down
          VERTICAL.flatMap((x, y) => puzzleMap.get(pos._1 + x, pos._2 - y)), // up
        ).map(_.mkString("") == "MAS")
      else Seq(false)
    }).count(p => p)
  }

  def partTwo(puzzle: Seq[String]): Int = {
    val coords = buildCoörds(puzzle)
    val puzzleMap: Map[(Int, Int), Char] = coords.toMap
    val ortho = (1, 1)

    coords.map((pos, c) => {
      if c == 'A' then
        val d1 = Seq(
          puzzleMap.get(pos._1 + ortho._1, pos._2 - ortho._2), // up right
          puzzleMap.get(pos._1 - ortho._1, pos._2 + ortho._2), // down left
        ).flatten.mkString("")

        val d2 = Seq(
          puzzleMap.get(pos._1 + ortho._1, pos._2 + ortho._2), // down right
          puzzleMap.get(pos._1 - ortho._1, pos._2 - ortho._2), // up left
        ).flatten.mkString("")

        (d1 == "SM" || d1 == "MS") && (d2 == "SM" || d2 == "MS")
      else false
    }).count(p => p)
  }

  println(s"the result of partOne is ${partOne(input)}")
  println(s"the result of partTwo is ${partTwo(input)}")


  /* Tests */

  val testInput: String =
    """MMMSXXMASM
      |MSAMXMSMSA
      |AMXSXMAAMM
      |MSAMASMSMX
      |XMASAMXAMM
      |XXAMMXXAMA
      |SMSMSASXSS
      |SAXAMASAAA
      |MAMMMXMMMM
      |MXMXAXMASX""".stripMargin

  def testPartOne(): Unit = assert(partOne(testInput.split("\n")) == 18)


  def testPartTwo(): Unit = {
    println(partTwo(testInput.split("\n")))
    assert(partTwo(testInput.split("\n")) == 9)
  }

  testPartOne()
  testPartTwo()

}
