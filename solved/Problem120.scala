object Problem120 {

  def r(a: Int, n: Int): Int = (BigInt(a - 1).pow(n) + BigInt(a + 1).pow(n)).mod(a * a).toInt

  def main(args: Array[String]) {
    println((3 to 1000).map({ a =>
      (1 to a * 2).map({ n => r(a, n) }).max
    }).sum)
  }
}
