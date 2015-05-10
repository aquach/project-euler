object Problem134 {
  val LIMIT = 1000 * 1000

  val sieve = {
    val SIEVE_LIMIT = LIMIT + 2000
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false

    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n) && n >= 5).toList
  }

  def extended_gcd(a : Long, b : Long) : (Long, Long) = {
    if( b == 0) return (1,0)
    else {
      val q = a/b
      val r = a - b*q
      val (s,t) = extended_gcd(b,r)
      return (t, s - q*t)
    }
  }

  def crt(a: Long, p: Long, b: Long, q: Long): Long = {
    val (x, y) = extended_gcd(p, q)
    val (pInvModQ, qInvModP) = ((x + q) % q, (y + p) % p)

    (a * q * qInvModP + b * p * pInvModQ) % (p * q)
  }

  def main(x: Array[String]) {
    println(
      sieve.sliding(2).dropWhile(_.head < 5)
        .takeWhile(_.head <= LIMIT).map({
        case List(p1, p2) => {
          val mod = p1 match {
            case x if x < 10 => 10
            case x if x < 100 => 100
            case x if x < 1000 => 1000
            case x if x < 10000 => 10000
            case x if x < 100000 => 100000
            case x if x < 1000000 => 1000000
          }
          crt(0, p2, p1, mod).toLong
        }
      }).sum
    )
  }
}
