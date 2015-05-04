object Problem172 {
  val DIGITS = 18
  def combos(digits: List[Int], currentSum: Int = 0): List[List[Int]] = (digits, currentSum) match {
    case (List(), DIGITS) => List(List())
    case (List(), _) => List()
    case (_, s) if DIGITS - s > digits.length * 3 => List()
    case (d :: ds, _) => (for (q <- (0 to 3).toList; outcome <- combos(ds, currentSum + q)) yield q :: outcome)
  }

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  val fact: Memo[Int, BigInt] = Memo {
    case 0 => 1
    case n => n * fact(n - 1)
  }

  def main(args: Array[String]) {
    println((
      for (combo <- combos((0 to 9).toList).toIterator) yield combo.foldLeft(fact(DIGITS))((f, c) => f / fact(c))
    ).sum * 9 / 10)
  }
}
