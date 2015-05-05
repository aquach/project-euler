object Problem110 {

  val primesList = {
    val SIEVE_LIMIT = 1000
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false

    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n))
  }

  def getPrimeFactorization(x: Int): Option[List[Int]] = {
    val (fac, result) = primesList.takeWhile(_ <= x / 2).foldLeft((List[Int](), x))((d, divisor) => {
      val (factors, i) = d
      if (i == 1) return Some(factors)

      if (i % divisor == 0) {
        val s = Stream.iterate(i)(_ / divisor).takeWhile(_ % divisor == 0)
        (
          List.fill(s.size)(divisor) ++ factors,
          s.last / divisor
        )
      } else {
        d
      }
    })
    if (result == 1)
      Some(fac)
    else
      None
  }

  val LIMIT = 4 * 1000 * 1000

  def main(x: Array[String]) {
    val results = Stream.from(LIMIT + 1).flatMap(solns => {
      val numPosFactors = solns * 2 - 1
      val factors = getPrimeFactorization(numPosFactors)
      factors.map(f => (solns, f))
    }).filter(l => l._2.nonEmpty && l._2.forall(_ < 25))
      .map({ case (solns, factors) => {
        val exponents = factors.map(_ / 2).sortBy(-_)
        val fac = primesList.zip(exponents)
        val result = fac.map(t => BigInt(t._1).pow(t._2)).reduce(_ * _)
        (result, solns, fac.map({ case (b, e) => s"$b^$e" }).mkString(" * "))
      }}).take(200)

    println(results.minBy(_._1))
  }
}
