object Problem136 {
  val LIMIT = 1000 * 1000 * 50

  val factorSieve = {
    val SIEVE_LIMIT = LIMIT

    val solutions = Array.fill(SIEVE_LIMIT)(0)

    Iterator.from(1).takeWhile(i => i * i < SIEVE_LIMIT).foreach { i =>
      println(i)
      (i*i until SIEVE_LIMIT by i).foreach(j => {
        if (solutions(j) <= 1) {
          val k = j / i
          if ((k + i) % 4 == 0) {
            if (valid(k, i)) {
              solutions(j) += 1
            }
            if (k != i && valid(i, k)) {
              solutions(j) += 1
            }
          }
        }
      })
    }

    solutions
  }

  @inline
  def valid(f1: Int, f2: Int) = {
    val x4 = 3 * f1 - f2
    val d4 = f1 + f2

    x4 > 0 && d4 > 0 && x4 % 4 == 0 && d4 % 4 == 0
  }

  def main(x: Array[String]) {
    println(factorSieve.count(_ == 1))
  }
}
