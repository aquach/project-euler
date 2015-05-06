object Problem131 {
  val LIMIT = 1000 * 1000

  val primesList = {
    val SIEVE_LIMIT = LIMIT
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false

    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n)).toSet
  }

  def main(args: Array[String]) {
    println(
      (1 to LIMIT).toIterator.map(x => BigInt(x).pow(3)).sliding(2)
        .map({ case List(l, r) =>  r - l })
        .takeWhile(_ < LIMIT)
        .filter(p => primesList(p.toInt)).size
    )
  }
}
