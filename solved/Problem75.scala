object Problem75 {

  val LIMIT = 1500000

  def gcd(a: Int, b: Int): Int = {
    b match {
      case 0 => a
      case _ => gcd(b, (a % b))
    }
  }

  def main(args: Array[String]) {
    println(
      (
        for (m <- Stream.from(1).iterator.takeWhile(m => 2 * m * m < LIMIT);
             n <- (1 until m).iterator if (m - n) % 2 == 1 && gcd(m, n) == 1;
             k <- Stream.from(1).iterator.takeWhile(2 * _ * m * (m + n) < LIMIT)
        ) yield {
          (k * (m * m - n * n), k * 2 * m * n, k * (m * m + n * n))
        }
      ).map({ case (a, b, c) => a + b + c }).toList.groupBy(identity).count(_._2.size == 1)
    )
  }
}
