println("Computing primes...")

val primesList = {
  val SIEVE_LIMIT = 40 * 1000 * 100
  val sieve = Array.fill(SIEVE_LIMIT)(true)
  sieve(0) = false
  sieve(1) = false

  Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
    (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
  }

  Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n))
}

val lastPrime = primesList.last

println(s"${primesList.length} primes.")
println(s"Going up to ${lastPrime}.")

println("Allocating totients..")

val totients = 0.to(lastPrime).toArray

println("Iterating through primes...")

primesList.foreach { p =>
  println(p)
  p.to(lastPrime).by(p).foreach { mult =>
    if (mult % 2 == 0) {
      totients(mult) = totients(mult) / p * (p - 1)
    }
  }
}

def chainLength(p: Int): Int = {
  Iterator.iterate(p)(a => totients(a)).takeWhile(_ != 1).length + 1
}

println("Computing chains..")

println(primesList.filter(p => chainLength(p) == 25).sum)
