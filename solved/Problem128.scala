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

def grid(q: Long, r: Long): Long = {
  val s = -q - r
  val ring = (Math.abs(q) + Math.abs(r) + Math.abs(s)) / 2
  val ringBase: Long = if (ring == 0) {
    1
  } else {
    2 + 3L * ring * (ring - 1)
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

val result = Stream.from(0, -1).flatMap { r =>
  0.to(1).flatMap { q =>
    val pd = List(
      grid(q, r - 1),
      grid(q - 1, r),
      grid(q - 1, r + 1),
      grid(q, r + 1),
      grid(q + 1, r),
      grid(q + 1, r - 1)
    ).map(n => Math.abs(grid(q, r) - n).toInt).count(sieve)

    if (pd == 3)
      Stream(grid(q, r))
    else
    Stream()
  }
}

println(result.take(2002)(1999))
