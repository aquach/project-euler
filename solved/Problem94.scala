import scala.collection.parallel.immutable.ParSet

object Problem94 {
  val LIMIT = 1000 * 1000 * 1000

  def isqrt(x: BigInt): BigInt = {
    if (x == 0) return 0
    val start = BigInt(2).pow(x.bitLength / 2)
    val s = Stream.iterate(start)(guess => (guess * guess + x) / 2 / guess)
    val (g1, g2) = s.zip(s.tail).dropWhile(Function.tupled((x, y) => (x - y).abs > 1)).head

    if (g1 == g2)
      g1
    else if (g1 * g1 > x)
      g2
    else
      g1
  }

  def isSquare(x: BigInt) = { 
    val d = x % 10
    (d == 0 || d == 5 || d == 1 || d == 6 || d == 9 || d == 4) && {
      val s = isqrt(x)
      s * s == x
    }
  }

  def getPerim(a: BigInt): BigInt = {
    def valid(x: BigInt): Boolean = {
      val (q, r) = x /% 16
      r == 0 && isSquare(q)
    }

    if (a % 1000000 == 1)
      println(a / 1000000)

    lazy val a3 = 3 * a
    lazy val am = a - 1
    val adigit = a % 10
    (
      if ((adigit == 1 || adigit == 5) && valid((a3 + 1) * am) && a3 + 1 <= LIMIT) {
        println("+", a, a3, (a3 + 1) * am)
        a3 + 1
      } else
        BigInt(0)
    ) +
    (
      if ((adigit == 1 || adigit == 7) && valid((a3 - 1) * (a + 1) * am * am) && a3 - 1 <= LIMIT) {
        println("-", a, a3, a3 - 1)
        a3 - 1
      } else
        BigInt(0)
    )
  }

  def main(args: Array[String]) {
    println(
      Stream.from(2).toIterator.filter({ x =>
        val d = x % 10
        d == 1 || d == 5 || d == 7
      }).map(BigInt(_)).takeWhile(3 * _ - 1 <= LIMIT).map(getPerim).sum
    )
  }
}
