println("Computing primes...")

val primesList = {
  val SIEVE_LIMIT = 40 * 1000// * 1000
  val sieve = Array.fill(SIEVE_LIMIT)(true)
  sieve(0) = false
  sieve(1) = false

  Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
    (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
  }

  Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n))
}

val primesSet = primesList.toSet

val lastPrime = primesList.last

println(s"${primesList.length} primes.")
println(s"Going up to ${lastPrime}.")

def totient(n: Int): Int = {
  if (primesSet(n))
    return n - 1

  if (n % 2 == 0) {
    val m = n / 2
    if (m % 2 == 0) {
      return 2 * mt(m)
    } else {
      return mt(m)
    }
  }

  val divisiblePrimes = primesList.iterator
    .takeWhile(_ <= n)
    .filter(n % _ == 0)

  divisiblePrimes.foldLeft(n)((n, prime) => n / prime * (prime - 1))
}

val expeconstant = Math.exp(0.57721566)

def maxBoundOnTotient(n: Int): Int = {
  println(n)
  val loglogn = Math.log(Math.log(n))
  (n * 1.0 / (expeconstant * loglogn)).toInt + 1
}


case class Memo[A, B](f: A => B) extends (A => B) {
  val cache = scala.collection.mutable.Map.empty[A, B]
  def apply(x: A) = cache.getOrElseUpdate(x, f(x))
}

val mt = Memo(totient _)

def chainLength(p: Int): Int = {
  // println(p)
  val a = Iterator.iterate(p)(a => mt(a)).takeWhile(_ != 1).length + 1
  // val b = Iterator.iterate(p)(maxBoundOnTotient).takeWhile(_ > 0).length + 1
  // println(p, a, b)
  a
}

def maxChainLength(p: Int): Int = {
  val a = Iterator.iterate(p)(maxBoundOnTotient).takeWhile(_ > 6).length
  println(p, a)
  a
}

val CHAIN_LENGTH = 25

// val bigEnoughPrimes = primesList.reverse.takeWhile(p => maxChainLength(p) >= CHAIN_LENGTH)
// println(s"Big enough primes: ${bigEnoughPrimes.length}")

// println(bigEnoughPrimes.filter(p => chainLength(p) == CHAIN_LENGTH).sum)

println(maxChainLength(39999983))
println(totient(4252))
