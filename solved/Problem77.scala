import scala.collection.mutable.MutableList
import scala.collection.mutable.Map

object Project77 {
  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  val LIMIT = 1000

  def main(args: Array[String]) {
    println("Filling array...")
    val sieve = Array.fill(LIMIT)(true)
    sieve(0) = false
    sieve(1) = false

    println("Computing primes...")

    Stream.from(2).takeWhile(i => i*i < LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until LIMIT by i).foreach(sieve(_) = false)
    }

    val primes = {
      val list = new MutableList[Int]
      var i = 0
      sieve.foreach { el =>
        if (sieve(i))
          list += i
        i += 1
      }

      list.toSeq
    }

    def successor(x: Int) = if (x == primes.head) None else Some(primes(primes.indexOf(x) - 1))

    lazy val numWays: Memo[(Int, Int), BigInt] = Memo {
      case (0, _) => 1
      case (total, maxIntToUse) =>
        successor(maxIntToUse).map(nextInt => numWays(total, nextInt)).getOrElse(BigInt(0)) + (
          if (total >= maxIntToUse) {
            numWays(total - maxIntToUse, maxIntToUse)
          } else {
            0
          }
        )
    }

    println(Stream.from(1).toIterator.map(n => (n, numWays((n, primes.last)) - (if (sieve(n)) 1 else 0))).dropWhile({ case (n, ways) =>
        println(n, ways)
        ways < 5000
      }).next)
  }
}
