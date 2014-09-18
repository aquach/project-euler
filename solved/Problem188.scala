object Problem188 {
  implicit val primesList = {
    val SIEVE_LIMIT = 100000
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

  // gcd(base, n) must equal 1.
  def hyperExpMod(base: BigInt, k: Int, n: Int) = {
    val reduction = totient(n)
    val chain = Iterator.iterate(base)(b => base.modPow(b % reduction, n)).take(k).toList
    chain.last
  }

  def main(x: Array[String]) {
    println(hyperExpMod(1777, 1855, 1000 * 1000 * 100))
  }
}
