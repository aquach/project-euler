import scala.collection.mutable.MutableList

object Problem70 {
  val LIMIT = 10000000

  def totient(n: Int, bestRatio: Double)(implicit primes: Seq[Int]): Option[Int] = {
    val divisiblePrimes = primes.iterator
      .takeWhile(_ <= n)
      .filter(n % _ == 0)

    var currentTotient = n
    divisiblePrimes.find(prime => {
      currentTotient = currentTotient / prime * (prime - 1)
      n >= bestRatio * currentTotient
    }) match {
      case None => Some(currentTotient)
      case _ => None
    }
  }

  def main(args: Array[String]) {

    println("Filling array...")
    val sieve = Array.fill(LIMIT)(true)
    sieve(0) = false
    sieve(1) = false

    println("Computing primes...")

    Stream.from(2).takeWhile(i => i*i < LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until LIMIT by i).foreach(sieve(_) = false)
    }

    implicit val primes: Seq[Int] = {
      val list = new MutableList[Int]
      var i = 0
      sieve.foreach { el =>
        if (sieve(i))
          list += i
        i += 1
      }

      list.toSeq
    }

    println("Computing result...")

    var bestGuess = (2, 100.0)
    (3 until LIMIT by 2).iterator
      .filter(!sieve(_))
      .filter(n =>
        n % 3 > 0 &&
        n % 5 > 0 &&
        n % 7 > 0 &&
        n % 11 > 0 &&
        n % 13 > 0 &&
        n % 17 > 0
      )
      .map(n => (n, totient(n, bestGuess._2)))
      .filter({
        case (n, Some(t_n)) => n.toString.toList.sorted == t_n.toString.toList.sorted
        case (n, _) => false
      })
      .foreach(t => { bestGuess = (t._1, t._1 * 1.0 / t._2.get); println(bestGuess) })
  }
}
