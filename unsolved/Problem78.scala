import scala.collection.mutable.Map

object Project76 {
  case class Memo[B](f: Tuple2[Int, Int] => B) extends (Tuple2[Int, Int] => B) {
    private val cache = scala.collection.mutable.Map.empty[(Int, Int), B]
    def apply(x: (Int, Int)): B = { 
      if (!cache.contains(x) && x._1 % 100 == 0)
        cache.retain({ case ((total, maxInt), value) => total + maxInt < x - 2 })
      println(cache)
      println("fetching " + x)
      cache.getOrElseUpdate(x, f(x))
    }
  }

  def main(args: Array[String]) {
    val coins = (1 to 10)

    def successor(x: Int) = x - 1
    lazy val numWays: Memo[BigInt] = Memo {
      case (0, _) => 1
      case (_, 1) => 1
      case (total, maxIntToUse) if (total < maxIntToUse) => numWays(total, total)
      case (total, maxIntToUse) =>
        numWays(total, successor(maxIntToUse)) + (
          if (total >= maxIntToUse) {
            numWays(total - maxIntToUse, maxIntToUse)
          } else {
            0
          }
        )
    }

    (1 to 10).foreach(x => println(numWays(x, x) + "\nHEY"))

    //println(Stream.from(1).toIterator
    //  .map(n => (n, numWays((n, coins.last))))
    //  .dropWhile({ case (n, ways) => { println(n, ways); ways % 1000000 != 0 } }).next)
  }
}
