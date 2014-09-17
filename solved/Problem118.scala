import scala.annotation.tailrec
import scala.collection.BitSet

object Problem118 {

  def intToDigits(d: Int): List[Int] = if (d < 10) List(d) else (d % 10) :: intToDigits(d / 10)

  val primesList = {
    val SIEVE_LIMIT = 1000 * 1000 * 100
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false
  
    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Iterator.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n)).filter(n => {
      val digits = intToDigits(n)
      !digits.exists(_ == 0) && digits.size == digits.toSet.size
    })
  }

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  val intToSet = Memo((d: Int) => intToDigits(d).toSet)

  def findAllSets(primes: List[Int], results: Set[Set[Int]] = Set(), currentSet: Set[Int] = Set()): Set[Set[Int]] = {
    val usedDigits = currentSet.flatMap(intToDigits)
    if (usedDigits.size == 9) {
      results + currentSet
    } else {
      val eligiblePrimes = primes.reverse.toIterator.zipWithIndex.takeWhile(s => {
        val primeDigits = intToSet(s._1)
        (primeDigits.size + usedDigits.size) <= 9
      }).filter(s => intToSet(s._1).forall(d => !usedDigits.contains(d)))
        .filter(s => currentSet.forall(_ > s._1))
      eligiblePrimes.toList.reverse.flatMap({ case (p, i) => findAllSets(primes.drop(i), results, currentSet + p) }).toSet
    }
  }

  def main(x: Array[String]) {
    val ps = primesList.toList
    println(findAllSets(ps.reverse).size)
  }
}
