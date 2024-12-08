import DaySix.Direction.Up

import scala.annotation.tailrec

object DaySix extends App {

  private val input = io.Source.fromResource("day_six.txt").getLines().toSeq

  enum Direction(val x: Int, val y: Int):
    case Up extends Direction(0, -1)
    case Down extends Direction(0, 1)
    case Left extends Direction(-1, 0)
    case Right extends Direction(1, 0)

  extension (d: Direction)
    def cart(c: (Int, Int)) = (c._1 + d.x, c._2 + d.y)
    def turnRight = d match
      case Direction.Up => Direction.Right
      case Direction.Right => Direction.Down
      case Direction.Down => Direction.Left
      case Direction.Left => Direction.Up

  def partOne(input: Seq[String]) = {
    val searchMap: Seq[Seq[Char]] = input.map(_.toCharArray.toSeq)
    val coords = for {
      y <- input.indices
      x <- 0 until input(y).length
    } yield (x, y) -> searchMap(y)(x)

    val start = coords.find((a, c) => c == '^').get._1
    val gridChecker = coords.toMap

    val result = play(start, gridChecker, Seq(start))
    result.toSet.size
  }

  def partTwo(input: Seq[String]) = {
    val searchMap: Seq[Seq[Char]] = input.map(_.toCharArray.toSeq)
    val coords = for {
      y <- input.indices
      x <- 0 until input(y).length
    } yield (x, y) -> searchMap(y)(x)

    val start = coords.find((a, c) => c == '^').get._1
    val gridChecker = coords.toMap

    val result = play(start, gridChecker, Seq(start))

    0 // todo :(
  }

  @tailrec
  def play(pos: (Int, Int), gridChecker: Map[(Int, Int), Char], res: Seq[(Int, Int)], dir: Direction = Up): Seq[(Int, Int)] = {
    val next = dir.cart(pos)
    gridChecker.get(next) match
      case Some(c) =>
        if c == '#' then play(pos, gridChecker, res, dir.turnRight)
        else play(next, gridChecker, res :+ next, dir)
      case None => res
  }

  println(s"the result for partOne is ${partOne(input)}")

  /* Tests */

  val testInput =
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin

  def testPartOne() = {
    val res = partOne(testInput.split("\n").toSeq)
    assert(res == 41)
  }

  def testPartTwo() = {
    val res = partTwo(testInput.split("\n").toSeq)
    assert(res == 6)
  }

  testPartOne()
  // testPartTwo()

}
