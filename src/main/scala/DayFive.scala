import scala.annotation.tailrec

object DayFive extends App {


  private val input = io.Source.fromResource("day_five.txt").mkString.split("\n").toSeq

  def partOne(rules: Seq[String], updates: Seq[String]) = {
    val order = rules.foldLeft(Map.empty[String, Seq[String]])((acc, next) => {
      val Array(key, nbf) = next.split('|')
      val lOrEm = acc.getOrElse(key, Seq.empty) :+ nbf
      acc + (key -> lOrEm)
    })

    updates.filter(s => {
      @tailrec
      def sublistContains(h: Option[String], t: Seq[String]): Boolean = {
        h match
          case Some(key) =>
            if t.isEmpty then true
            else if order.getOrElse(key, Seq.empty).intersect(t).isEmpty then
              sublistContains(t.headOption, t.tail)
            else
              false
          case None => true
      }

      val checkOrder = s.split(',').reverse.toSeq
      sublistContains(checkOrder.headOption, checkOrder.tail)
    }).map(s =>
      val arr = s.split(',').toSeq
      val pad = if arr.length % 2 == 0 then -1 else 0
      val mid = arr.length / 2 + pad
      arr.zipWithIndex.filter((x, i) => i == mid).head._1.toInt
    ).sum
  }

  private val i = input.indexWhere(s => s.isBlank)
  private val (rules, updates) = input.splitAt(i)
  println(s"result for part one is ${partOne(rules, updates.tail)}")

  /* Tests */

  val testInput =
    """47|53
      |97|13
      |97|61
      |97|47
      |75|29
      |61|13
      |75|53
      |29|13
      |97|29
      |53|29
      |61|53
      |97|53
      |61|29
      |47|13
      |75|47
      |97|75
      |47|61
      |75|61
      |47|29
      |75|13
      |53|13
      |
      |75,47,61,53,29
      |97,61,53,29,13
      |75,29,13
      |75,97,47,61,53
      |61,13,29
      |97,13,75,29,47""".stripMargin

  def testPartOne(): Unit = {
    val s = testInput.split("\n").toSeq
    val i = s.indexWhere(s => s.isBlank)
    val (one, two) = s.splitAt(i)
    assert(partOne(one, two.tail) == 143)
  }

  testPartOne()
}
