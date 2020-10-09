val MOD = BigInt("500500507")

println("Computing primes...")

val sieve = {
  val SIEVE_LIMIT = 10000000
  val sieve = Array.fill(SIEVE_LIMIT)(true)
  sieve(0) = false
  sieve(1) = false

  Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
    (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
  }

  Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n))
}

println(s"Computed ${sieve.length} primes.")

println("Done.")

def number(twoExponents: Iterator[Int]): BigInt = {
  twoExponents.zip(sieve).map {
    case (e, prime) => BigInt(prime).pow(BigInt(2).pow(e).toInt - 1)
  }.reduce(_ * _)
}

def numberMod(twoExponents: Iterator[Int]): BigInt = {
  twoExponents.zip(sieve).map {
    case (e, prime) => BigInt(prime).modPow(BigInt(2).pow(e).toInt - 1, MOD)
  }.reduce((a, b) => (a * b).mod(MOD))
}

def lnNumber(twoExponents: Iterator[Int]): BigDecimal = {
  twoExponents.zip(sieve).map {
    case (e, prime) => BigDecimal((BigInt(2).pow(e) - 1)) * Math.log(prime)
  }.reduce(_ + _)
}

def solve(twoToNumDivs: Int): BigInt = {
  assert(twoToNumDivs <= sieve.length)

  val twoExponents = scala.collection.mutable.ArrayBuffer.empty[Int]
  twoExponents ++= Array.fill(twoToNumDivs)(1)

  var didWork = true

  while (didWork) {
    // println(twoExponents.length, twoExponents, lnNumber(twoExponents.toIterator))
    didWork = false

    val lastPrime = sieve(twoExponents.length - 1)

    var i = 0

    while (i < twoExponents.length - 1 && !didWork && !(i > 0 && twoExponents(i - 1) == 1)) {
      val currentPrime = sieve(i)

      val amount = (Math.log(Math.log(lastPrime) / Math.log(currentPrime)) / Math.log(2)).toInt - twoExponents(i) + 1

      if (amount > 0) {
        didWork = true

        twoExponents.dropRightInPlace(1)
        twoExponents(i) += 1
      }

      i += 1
    }

    // println()
  }

  println("Done. Now computing number.")

  // println(twoExponents)
  // println(number(twoExponents.toIterator))

  numberMod(twoExponents.toIterator)
}

println(solve(500500))

def slowFactors(n: Long) = {
  var factors = 0

  var i = 1
  val lim = Math.sqrt(n).toLong + 1L

  while (i < lim) {
    if (n % i == 0) {
      factors += 1
    }
    i += 1
  }

  factors * 2
}

// println(slowFactors(294053760L))
