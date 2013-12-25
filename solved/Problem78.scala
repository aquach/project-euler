import scala.collection.mutable.Map

object Problem78 {
  case class Memo[B](f: Int => B) extends (Int => B) {
    private val cache = scala.collection.mutable.Map.empty[Int, B]
    def apply(x: Int): B = { 
      cache.getOrElseUpdate(x, f(x))
    }
  }

  def main(args: Array[String]) {
    def p(x: Int) = x * (3 * x - 1) / 2
    val g = Stream.from(1).flatMap(x => Stream(p(x), p(-x)))

    lazy val numWays: Memo[BigInt] = Memo {
      case x if x < 0 => 0
      case 0 => 1
      case 1 => 1
      case x => 
        g.takeWhile(_ <= x).zipWithIndex.map(
          Function.tupled((p, i) => (if ((i / 2) % 2 == 0) 1 else -1) * numWays(x - p))).sum
    }

    println(Stream.from(1).map(x => (x, numWays(x))).find(_._2 % 1000000 == 0))
  }
}
