object Problem116 {

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  val fact: Memo[Int, BigInt] = Memo {
    case 0 => 1
    case n => n * fact(n - 1)
  }

  def numWays(L: Int): BigInt = 
    ((2 to 4).map { tileLength =>
      ((1 to L / tileLength).map { numTiles =>
        fact(L - (tileLength - 1) * numTiles) / fact(L - tileLength * numTiles) / fact(numTiles)
      }).sum
    }).sum

  def main(args: Array[String]) {
    println(numWays(50))
  }
}
