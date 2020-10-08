val MOD = BigInt("500500507")

println("Computing primes...")

val sieve = {
  val SIEVE_LIMIT = 100000
  val sieve = Array.fill(SIEVE_LIMIT)(true)
  sieve(0) = false
  sieve(1) = false

  Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
    (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
  }

  Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n))
}

println(sieve.last)
println(sieve.length)


println("Done.")

def number(twoExponents: Iterator[Int]): BigInt = {
  twoExponents.zip(sieve).map {
    case (e, prime) => BigInt(prime).pow(BigInt(2).pow(e).toInt - 1)
  }.reduce(_ * _)
}

def lnNumber(twoExponents: Iterator[Int]): BigDecimal = {
  twoExponents.zip(sieve).map {
    case (e, prime) => BigDecimal((BigInt(2).pow(e) - 1)) * Math.log(prime)
  }.reduce(_ + _)
}

def solve(twoToNumDivs: Int): BigInt = {
  val twoExponents = scala.collection.mutable.ArrayBuffer.empty[Int]
  twoExponents += twoToNumDivs

  var didWork = true

  while (didWork) {
    println(twoExponents)
    didWork = false

    var i = 0

    while (i < twoExponents.length) {
      println(i)
      val currentMax = lnNumber(twoExponents.toIterator)

      val twoEx1 = twoExponents(i)
      if (i == twoExponents.length - 1 && twoEx1 > 1) {
        println("Considering adding a new entry.")
        val a = twoExponents.toArray.dropRight(1) ++ Array(twoEx1 - 1, 1)
        println("PROPOSAL", a.toList)
        if (lnNumber((twoExponents.toArray.dropRight(1) ++ Array(twoEx1 - 1, 1)).toIterator) < currentMax) {
          println("Smaller!")
          didWork = true

          twoExponents(i) -= 1
          twoExponents += 1
        } else {
          println("Bigger :(")
        }
      } else if (i < twoExponents.length - 1) {
        println("Considering shifting a value over.")
        val proposal = twoExponents.toArray
        proposal(i) -= 1
        proposal(i + 1) += 1
        println("PROPOSAL", proposal.toList)
        if (lnNumber(proposal.toIterator) < currentMax) {
          println("Smaller!")
          didWork = true

          twoExponents(i) -= 1
          twoExponents(i + 1) += 1
        } else {
          println("Bigger")
        }
      }

      println("now", twoExponents.toList)
      i += 1
    }

    println()
  }

  println(twoExponents)
  println(number(twoExponents.toIterator))

  number(twoExponents.toIterator).mod(MOD)
}

println(solve(9))

// Switch to all-prime method and aggregate rather than all 2s and split.
