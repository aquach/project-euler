object Problem204 {
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

  val LIMIT = 1000 * 1000 * 1000

  def g(primesList: List[Long], limit: Long): Long = primesList match {
    case List(p) => (math.log(limit) / math.log(p)).toInt + 1
    case p :: ps => {
      val powers = Iterator.iterate(1L)(_ * p).takeWhile(_ <= limit).toList
      powers.map(power => g(ps, limit / power)).sum
    }
  }

  def main(x: Array[String]) {
    println(g(primesList.toList.reverse.map(_.toLong), LIMIT))
  }
}
