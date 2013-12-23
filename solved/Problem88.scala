object Problem88 {

  def getPrimeFactorization(x: Int): List[Int] =
    (2 to x / 2).foldLeft((List[Int](), x))((d, divisor) => {
      val (factors, i) = d

      if (i % divisor == 0) {
        val s = Stream.iterate(i)(_ / divisor).takeWhile(_ % divisor == 0)
        (
          List.fill(s.size)(divisor) ++ factors,
          s.last / divisor
        )
      } else {
        d
      }
    })._1

  def aggregateFactors(factors: List[Int]): Seq[List[Int]] = {
    (for (
      x <- (0 until factors.size);
      rest = factors.patch(x, Nil, 1);
      y <- (0 until rest.size)
    ) yield {
      val newNum = factors(x) * rest(y)
      (newNum +: rest.patch(y, Nil, 1)).sorted
    }).distinct
  }


  def computePossibleSums(factors: List[Int]): Map[Int, Set[Int]] = {
    val len = factors.size
    val s = Stream.iterate(Seq(factors))(_.map(aggregateFactors).flatten.distinct)
    s.take(len).zipWithIndex.map({ d =>
      val (combinations, i) = d

      (len - i, combinations.map(_.sum).toSet)
    }).toMap
  }

  def main(args: Array[String]) {
    val lookup = (2 to 13000).map(getPrimeFactorization _ andThen computePossibleSums _).toVector
    println((for (
      k <- (2 to 12000).iterator;
      min = Stream.from(2).find(x => lookup(x - 2).find(Function.tupled(
        (numFactors, possibleSums) => possibleSums.contains(x - k + numFactors))).isDefined).get
    ) yield {
      min
    }).toSet.sum)
  }
}
