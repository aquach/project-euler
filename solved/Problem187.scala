import scala.collection.mutable.MutableList

object Problem187 {

  val LIMIT = 1000 * 1000 * 100
  //val LIMIT = 30

  def main(args: Array[String]) {
    println("Filling array...")
    val sieve = Array.fill(LIMIT)(true)
    sieve(0) = false
    sieve(1) = false
  
    println("Computing primes...")

    Stream.from(2).iterator.takeWhile(i => i*i < LIMIT / 2).filter(sieve(_)).foreach { i =>
      (i*i until LIMIT / 2 by i).foreach(sieve(_) = false)
    }

    println("Computing...")

    println(
      (for (i <- (0 until LIMIT).iterator if sieve(i);
            j <- (0 until LIMIT).iterator.takeWhile(i * _ < LIMIT) if sieve(j)) yield { i * j }
      ).toSet.size)
  }
}
