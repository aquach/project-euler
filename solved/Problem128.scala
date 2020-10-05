println("Computing primes...")

val sieve = {
  val SIEVE_LIMIT = 1000000
  val sieve = Array.fill(SIEVE_LIMIT)(true)
  sieve(0) = false
  sieve(1) = false

  Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
    (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
  }

  Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n)).toSet
}

println("Done.")

case class Memo[A, B](f: A => B) extends (A => B) {
  private val cache = scala.collection.mutable.Map.empty[A, B]
  def apply(x: A) = cache.getOrElseUpdate(x, f(x))
}

def grid(q: Int, r: Int): BigInt = {
  val s = -q - r
  val ring = BigInt((Math.abs(q) + Math.abs(r) + Math.abs(s)) / 2)
  val ringBase: BigInt = if (ring == 0) {
    1
  } else {
    2 + BigInt(3) * ring * (ring - 1)
  }

  ringBase + (
    if (s == ring) {
      -q
    } else if (q == -ring) {
      ring + r
    } else if (r == ring) {
      ring * 2 - s
    } else if (s == -ring) {
      ring * 3 + q
    } else if (q == -ring) {
      ring * 4 - r
    } else {
      ring * 5 + s
    }
  )
}

val mgrid = Memo[(Int, Int), BigInt]({ case (q, r) => grid(q, r) })

val result = for (
  q <- 0.to(1);
  r <- (-100000).to(0);
  pd = List(
    mgrid(q, r - 1),
    mgrid(q - 1, r),
    mgrid(q - 1, r + 1),
    mgrid(q, r + 1),
    mgrid(q + 1, r),
    mgrid(q + 1, r - 1)
  ).map(n => (mgrid(q, r) - n).abs.toInt).count(sieve) if pd == 3
) yield {
  mgrid(q, r)
}

println(result.sorted.apply(1999))
