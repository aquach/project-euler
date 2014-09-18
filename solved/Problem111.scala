object Problem111 {
  val primesList = {
    val SIEVE_LIMIT = 1000
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false
  
    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Iterator.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n))
  }

  def isPrime(n: Long) = (2 to math.sqrt(n).toInt).forall(n % _ != 0)

  def allPossibilities(xs: List[Int], n: Int): List[List[Int]] =
    if (n <= 0)
      List(List())
    else
      for (poss <- allPossibilities(xs, n - 1); x <- xs) yield x :: poss

  def s(n: Int, d: Int) = Iterator.from(n - 1, -1).map(numRepeated =>
    allPossibilities(((0 to 9).toSet - d).toList, n - numRepeated)
      .map(p => List.fill(numRepeated)(d) ++ p)
      .flatMap(_.permutations)
      .filter(_.head != 0)
      .map(_.mkString.toLong)
      .toSet
      .filter(isPrime)
  ).find(_.nonEmpty).get.sum

  def main(x: Array[String]) {
    println((0 to 9).map(s(10, _)).sum)
  }
}
