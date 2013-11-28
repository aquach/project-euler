object Problem60 {
  
  val LIMIT = 1200000000

  def main(args: Array[String]) {
    println("Filling array...")
    val sieve = Array.fill(LIMIT)(true)
    sieve(0) = false
    sieve(1) = false
  
    println("Computing primes...")

    Stream.from(2).takeWhile(i => i*i < LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until LIMIT by i).foreach(sieve(_) = false)
    }

    println("Selecting primes...")
    val primes = new Iterator[Int] {
      private var i = 0

      def hasNext = true
      def next = {
        i += 1
        while (!sieve(i)) {
          i += 1
        }
        i
      }
    }.take(1300).toList
    val startingCandidates = primes.map(Set(_))

    val results = (0 until 4).foldLeft(startingCandidates)((candidates, _) =>
      (
        for (candidateSet <- candidates.par;
             prime <- primes
             if (candidateSet.forall(c => {
                val a = (c.toString + prime.toString).toInt
                val b = (prime.toString + c.toString).toInt
                sieve(a) &&
                sieve(b)
             }))
        ) yield {
          candidateSet + prime
        }
      ).toList.distinct
    )
    println(results)
  }
}
