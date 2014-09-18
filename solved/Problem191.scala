object Problem191 {

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  def prizes(numConsecAbsent: Int, numLate: Int, daysLeft: Int): Int =
    (numConsecAbsent, numLate, daysLeft) match {
      case (3, _, _) | (_, 2, _) => 0
      case (_, _, 0) => 1
      case (nc, nl, days) => 
        prizesMemoed(nc + 1, nl, days - 1) +
        prizesMemoed(0, nl + 1, days - 1) +
        prizesMemoed(0, nl, days - 1)
    }

  val prizesMemoed = Memo((prizes _).tupled)

  def main(x: Array[String]) {
    println(prizesMemoed(0, 0, 30))
  }
}
