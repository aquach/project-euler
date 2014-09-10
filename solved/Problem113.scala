object Problem113 {
  val DIGITS = 100

  def printGrid(g: Array[Array[Long]]) {
    g.foreach { r =>
      r.foreach { e =>
        print(e.toString.padTo(4, ' ') + ' ')
      }
      println()
    }
  }

  def dp(lastDigitNonZero: Boolean): Long = {
    // g(n, d) = # of nos with `n + 1` digits that end in the digit `d` and are increasing or decreasing depending on lastDigitNonZero.
    // The situations are symmetric in calculation; the only difference is leading zeroes.
    val g = Array.fill(DIGITS, 10)(0L)
    g(0) = Array.fill(10)(1) // g(0)(0) is not marked 0 because it's a valid digit to build new numbers from.
    (1 until DIGITS).foreach { r =>
      (0 until 10).foreach { d =>
        // lastDigitNonZero == true forbids adding zeroes to the beginning of numbers, which prevents leading zeroes.
        g(r)(d) = ((if (lastDigitNonZero) 1 else 0) to d).map(x => g(r - 1)(x)).sum
      }
    }
    // When lastDigitZero == false, 0, 00, 000, etc are all considered valid, and so we subtract out these extra values.
    g.map(_.sum).sum - (if (lastDigitNonZero) 0 else DIGITS.toLong)
  }

  def main(x: Array[String]) {
    // The 10 single digit numbers and the numbers that are all the same digit are 
    // are both increasing and decreasing, so we need to undoublecount them.
    println(dp(true) + dp(false) - 10 - (DIGITS.toLong - 1) * 9)
  }
}
