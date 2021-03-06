import scala.collection.mutable.Map

object Problem31 {
  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  def main(args: Array[String]) {
    val coins = Array[Int](1, 2, 5, 10, 20, 50, 100, 200)

    def successor(x: Int) = coins(coins.indexOf(x) - 1)

    lazy val numWays: Memo[(Int, Int), BigInt] = Memo {
      case (0, _) => 1
      case (_, 1) => 1
      case (total, maxIntToUse) =>
        numWays(total, successor(maxIntToUse)) + (
          if (total >= maxIntToUse) {
            numWays(total - maxIntToUse, maxIntToUse)
          } else {
            0
          }
        )
    }

    println(numWays(200, coins.last))
  }
}
