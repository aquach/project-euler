import scala.collection.parallel.immutable.ParSet

object Problem94 {
  val LIMIT = 1001 * 1000 * 1000

  def isqrt(x: Long): Long = {
    val s = Stream.iterate(x.toDouble)(guess => (guess + x / guess) / 2)
    s.zip(s.tail).dropWhile(Function.tupled((x, y) => math.abs(x - y) >= 0.3)).head._2.toLong
  }

  def isSquare(x: Long) = { 
    val s = isqrt(x)
    s * s == x
  }

  def getPerim(a: Int) = {
    def valid(x: Long) = x % 16 == 0 && isSquare(x / 16)

    if (a % 1000000 == 1)
      println(a / 1000000)

    (
      if (valid((3L * a + 1) * (a - 1)) && 3 * a + 1 <= LIMIT) {
        println("+", a)
        3 * a + 1
      } else
        0
    ) +
    (
      if (valid((3L * a - 1) * (a + 1) * (a - 1) * (a - 1)) && 3 * a - 1 <= LIMIT) {
        println("-", a)
        3 * a - 1
      } else
        0
    )
  }

  def slowValid(x: Int) = {
    val c = x - 1
    val s = (x + x + c) / 2.0
    val total = s * (s - x) * (s - x) * (s - c)
    val a = math.sqrt(total)
    a.toLong * a.toLong == total
  }

  def main(args: Array[String]) {
    //Stream.from(1).toIterator.filter(slowValid).foreach(println)
    println(
      Stream.from(1).toIterator.takeWhile(3 * _ - 1 <= LIMIT).map(getPerim).sum
    )
  }
}
