import scala.collection.mutable.MutableList

object Problem243 {
  val LIMIT = 15499.0 / 94744

  val primesList = {
    val SIEVE_LIMIT = 100
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false
  
    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n))
  }

  def totient(n: Int)(implicit primes: Seq[Int]): Int = {
    val divisiblePrimes = primes.iterator
      .takeWhile(_ <= n)
      .filter(n % _ == 0)

    divisiblePrimes.foldLeft(n)((n, prime) => n / prime * (prime - 1))
  }
  
  def r(n: Int) = totient(n)(primesList) * 1.0 / (n - 1)

  def main(args: Array[String]) {
    val smallestPrimeList = primesList.toList.inits.toList.reverse.tail.find(ps =>
      ps.map(p => (p - 1) * 1.0 / p).reduce(_ * _) < LIMIT).get
    println(smallestPrimeList.reduce(_ * _))
    println(r(223092870 * 4) < LIMIT)
  }
}
