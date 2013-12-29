object Problem179 {

  val LIMIT = 10000000

  def main(args: Array[String]) {
    val divisors = Array.fill(LIMIT)(1)

    for (n <- 1 until LIMIT; j <- n * 2 until LIMIT by n) {
      divisors(j) += 1
    }

    println(divisors.sliding(2).filter(pair => {
      pair(0) == pair(1)
    }).size)
  }
}
