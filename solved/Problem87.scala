import scala.collection.mutable.MutableList

object Problem87 {
  
  val PRIME_LIMIT = 10000
  val LIMIT = 50000000

  def main(args: Array[String]) {
    println("Filling array...")
    val sieve = Array.fill(PRIME_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false

    println("Computing primes...")

    Stream.from(2).takeWhile(i => i*i < PRIME_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until PRIME_LIMIT by i).foreach(sieve(_) = false)
    }

    val primes: Seq[Int] = {
      val list = new MutableList[Int]
      var i = 0
      sieve.foreach { el =>
        if (sieve(i))
          list += i
        i += 1
      }

      list.toSeq
    }

    println(
      (
        for (a <- primes.iterator;
             b <- primes.iterator.takeWhile(b => b * b * b + a * a < LIMIT);
             c <- primes.iterator.takeWhile(c => c * c * c * c + b * b * b + a * a < LIMIT)
           ) yield {
           c * c * c * c + b * b * b + a * a
        }
      ).toSet.size
  )
  }
}
