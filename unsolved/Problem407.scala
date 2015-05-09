object Problem407 {

  val LIMIT = 1000 * 100
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
      (LIMIT - 1 to LIMIT).map(n => {
          println(n)

        if (sieve(n))
          1
        else
          (n to 0 by -1).filterNot(sieve).find(a => (a * a) % n == a).get
      })
    )
  }
}
