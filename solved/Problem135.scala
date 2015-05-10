object Problem135 {
  val LIMIT = 1000 * 1000
  val DESIRED = 10

  def factorPairs(n: Int) = (1 to Math.sqrt(n).toInt + 1).filter(n % _ == 0).map(f => (f, n / f))

  def valid(f1: Int, f2: Int) = {
    val x4 = 3 * f1 - f2
    val d4 = f1 + f2

    x4 % 4 == 0 && d4 % 4 == 0 && x4 > 0 && d4 > 0
  }

  def main(x: Array[String]) {
    println(
      (1 until LIMIT).count(n => {
        val factors = factorPairs(n)
        factors.flatMap({
          case (f1, f2) if f1 == f2 => List(valid(f1, f1))
          case (f1, f2) => List(valid(f1, f2), valid(f2, f1))
        }).count(x => x) == DESIRED
      })
    )
  }
}
