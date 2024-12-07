object DayFive extends App {
  private val input = io.Source.fromResource("day_five.txt").getLines().toSeq

  private def createOrder(rules: Seq[String]) = rules.foldLeft(Map.empty[String, Seq[String]])((acc, next) => {
    val Array(key, nbf) = next.split('|')
    val lOrEm = acc.getOrElse(key, Seq.empty) :+ nbf
    acc + (key -> lOrEm)
  })

  def partOne(rules: Seq[String], updates: Seq[String]) = {
    val order = createOrder(rules)

    updates.filter(s => {
      s.split(',')
        .reverse
        .toSeq
        .tails
        .forall(t => t == Nil || order.getOrElse(t.head, Seq.empty).intersect(t.tail).isEmpty)
    }).map(s =>
      val arr = s.split(',').toSeq
      arr(arr.size / 2).toInt
    ).sum
  }

  def partTwo(rules: Seq[String], updates: Seq[String]) = {
    val order = createOrder(rules)

    updates
      .filterNot(s => {
        s.split(',')
          .reverse
          .toSeq
          .tails
          .forall(t => t == Nil || order.getOrElse(t.head, Seq.empty).intersect(t.tail).isEmpty)
      })
      .map(s => s.split(',').toSeq.sortWith((a, b) => {
        order.getOrElse(b, Seq.empty).contains(a)
      }))
      .map(arr => arr(arr.size / 2).toInt).sum
  }

  private val i = input.indexWhere(s => s.isBlank)
  private val (rules, updates) = input.splitAt(i)
  println(s"result for part one is ${partOne(rules, updates.tail)}")
  println(s"result for part two is ${partTwo(rules, updates.tail)}")

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

  def testPartTwo(): Unit = {
    val s = testInput.split("\n").toSeq
    val i = s.indexWhere(s => s.isBlank)
    val (one, two) = s.splitAt(i)
    assert(partTwo(one, two.tail) == 123)
  }

  testPartOne()
  testPartTwo()
}
