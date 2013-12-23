object Problem88 {

  def getPrimeFactorization(x: Int): List[Int] =
    (2 to x / 2).foldLeft((List[Int](), x))((d, divisor) => {
      val (factors, i) = d

      if (i % divisor == 0) {
        val s = Stream.iterate(i)(_ / divisor).takeWhile(_ % divisor == 0)
        (
          List.fill(s.size)(divisor) ++ factors,
          s.last
        )
      } else {
        d
      }
    })._1

  def computeSums(factors: List[Int]): Set[Int] = {
    def computeSums(factors: Vector[Int]): Vector[Int] = 
      factors.sum +:
        (for (
          x <- (0 until factors.size);
          rest = factors.patch(x, Nil, 1);
          y <- (0 until rest.size)
        ) yield {
        println(x, y)
          val newNum = factors(x) * rest(y)
          computeSums(newNum +: rest.patch(y, Nil, 1))
        }).flatten.toVector

    computeSums(factors.toVector).toSet
  }

  def main(args: Array[String]) {
    println(computeSums(getPrimeFactorization(60)))
    //val lookup = (2 to 1000).map(((x: Int) => { println(x); x }) andThen getPrimeFactorization _ andThen computeSums _).toVector
    //println(lookup)
  }
}
