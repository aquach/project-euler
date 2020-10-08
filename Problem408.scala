val LIMIT = 1007
val MOD = BigInt("1000000007")

println("Computing triples...")
val LIMIT_SQRT = Math.sqrt(LIMIT).toInt

case class Memo[A, B](f: A => B) extends (A => B) {
  private val cache = scala.collection.mutable.Map.empty[A, B]
  def apply(x: A) = cache.getOrElseUpdate(x, f(x))
}

def gcd(a: Int, b: Int): Int = {
  b match {
    case 0 => a
    case _ => gcd(b, (a % b))
  }
}

val mgcd = Memo[(Int, Int), Int] {
  case (a, b) => gcd(a, b)
}

val ineligibleBuf = new scala.collection.mutable.ArrayBuffer[(Int, Int)](5000)

var m = 1

while (m <= LIMIT_SQRT) {
  var n = 1
  while (n < m) {
    if (!(m % 2 == 1 && n % 2 == 1) && mgcd(m, n) == 1) {
      val maxK = LIMIT_SQRT / Math.max(m, n)
      1.to(maxK).foreach { k =>
        val a = k * (m * m - n * n)
        val b = k * 2 * m * n
        if (a * a <= LIMIT && b * b <= LIMIT) {
          ineligibleBuf += ((a * a, b * b))
        }
      }
    }

    n += 1
  }

  m += 1
}

val ineligiblePoints = ineligibleBuf.toSet

val manual = for {
x <- 1.to(LIMIT)
y <- x.to(LIMIT) if Math.sqrt(x).toInt * Math.sqrt(x).toInt == x && Math.sqrt(y).toInt * Math.sqrt(y).toInt == y && Math.sqrt(x + y).toInt * Math.sqrt(x + y).toInt == x + y
} yield {
(x, y)
}

// val manual = List((2, 2))

println(manual.toList.sorted)
println(manual.length)
println(ineligiblePoints.map(_.productIterator.toSet).toSet == manual.map(_.productIterator.toSet).toSet)

def fact(i: Int): BigInt = {
  if (i <= 1)
    return BigInt(1)
  1.to(i).map(BigInt(_)).reduce(_ * _)
}

val N = LIMIT - 1

val allPaths = fact(N * 2) / fact(N) / fact(N)

def pascal(x: Int, y: Int): BigInt = fact(x + y) / fact(x) / fact(y)

val eliminatedPaths = manual.map {
  case (i, n) => 2 * pascal(i, n) * pascal(N - i, N - n)
}.sum

println(allPaths)
println(eliminatedPaths)

println((allPaths - eliminatedPaths).mod(MOD))
