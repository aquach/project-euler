object Problem120 {

  def r(a: Int, n: Int): Int = (BigInt(a - 1).pow(n) + BigInt(a + 1).pow(n)).mod(a * a).toInt

  def main(args: Array[String]) {
    println((3 to 1000).map({ a =>
      (0 to 100).foreach({ n => println(a, n, r(a, n)) }) 
      12
    }).sum)
  }
}
