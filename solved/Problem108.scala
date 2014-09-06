object Problem108 {

  val LIMIT = 1000

  val sieve = {
    val SIEVE_LIMIT = 10000
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false
  
    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n)).toSet
  }

  val primesList = sieve.toList.sorted

  def getPrimeFactorization(x: Int): List[(Int, Int)] =
    primesList.takeWhile(s => s * s <= x).foldLeft((List[(Int, Int)](), x))((d, divisor) => {
      val (factors, i) = d

      if (i % divisor == 0) {
        val s = Stream.iterate(i)(_ / divisor).takeWhile(_ % divisor == 0)
        (
          (divisor, s.size) :: factors,
          s.last / divisor
        )
      } else {
        d
      }
    })._1

  def numFactorPairsOfNSquared(n: Int) =
    getPrimeFactorization(n).map(_._2).map(_ * 2 + 1).reduceOption(_ * _).getOrElse(1) / 2 + 1

  def main(args: Array[String]) {
    println(
      Iterator.from(1)
      .filter(n => !sieve.contains(n))
      .map(n => (n, numFactorPairsOfNSquared(n)))
      .find(_._2 > LIMIT).get._1
    )
  }
}
