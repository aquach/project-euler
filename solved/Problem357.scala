object Problem357 {

  val LIMIT = 1000 * 1000 * 100

  val sieve = {
    val SIEVE_LIMIT = LIMIT + 1
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false

    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    sieve
  }

  def main(args: Array[String]) {
    println(
      (1 until LIMIT)
      .filter(n => sieve(n + 1))
      .filter(n => sieve(n / 2 + 2))
      .filter(n => n % 3 != 0 || sieve(n / 3 + 3))
      .filter(n => n % 4 != 0 || sieve(n / 4 + 4))
      .filter(n => n % 5 != 0 || sieve(n / 5 + 5))
      .filter(n => (1 to Math.sqrt(n).toInt).forall(d => n % d != 0 || sieve(d + n / d)))
      .map(n => {
        BigInt(n)
      })
      .sum
    )
  }
}
