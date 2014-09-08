object Problem123 {

  val primesList = {
    val SIEVE_LIMIT = 1000 * 1000
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false
  
    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n))
  }

  def r(n: Int, p: Int) = {
    val pn = BigInt(p)
    val mod = pn * pn
    ((pn - 1).modPow(n, mod) + (pn + 1).modPow(n, mod)) % mod
  }

  def r2(n: Int, p: Int) = if (n % 2 == 0) BigInt(2) else BigInt(2) * n * p

  val LIMIT = BigInt(10).pow(10)

  def main(x: Array[String]) {
    println(primesList.toIterator.zipWithIndex
      .map({ case (p, n) => (n + 1, r2(n + 1, p)) })
      .find(_._2 > LIMIT).get)
  }
}
