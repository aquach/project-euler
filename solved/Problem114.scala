object Problem114 {

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  val fit: Memo[Int, Long] = Memo((limit: Int) => 
    (3 to limit).map(l1 => {
      val leftGaps = 0 to (limit - l1)
      val rightGaps = leftGaps.map(limit - l1 - 1 - _)
      rightGaps.map(fit).sum
    }).sum + 1 // Empty case, where we don't place a block.
  )

  def main(x: Array[String]) {
    println(fit(50))
  }
}
