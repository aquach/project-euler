object Problem76 {

  def main(args: Array[String]) {
    val numWays = Array.fill(101)(0)
    numWays(1) = 0
    numWays(2) = 1

    for (n <- 3 to 100) {
      numWays(n) = (1 to n / 2).map({ a =>
        val b = n - a
        println((n, a, b))
        if (a != b)
          numWays(a) + numWays(b) + 1
        else
          numWays(a) + 1
      }).sum
      println("num ways to sum to " + n + ": " + numWays(n))
    }

    println(numWays(100))
  }
}
