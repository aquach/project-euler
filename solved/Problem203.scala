object Problem203 {
  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  val fact: Memo[Int, BigInt] = Memo {
    case 0 => 1
    case n => n * fact(n - 1)
  }

  val primesList = {
    val SIEVE_LIMIT = 200
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false
  
    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n))
  }

  def isSquareFree(x: BigInt): Boolean = {
    primesList.takeWhile(_ <= x / 2).foldLeft(x)((d, divisor) => {
      if (d == 1) return true

      if (d % divisor == 0) {
        val s = Stream.iterate(d)(_ / divisor).takeWhile(_ % divisor == 0)
        if (s.size > 1)
          return false
        s.last / divisor
      } else {
        d
      }
    })
    true
  }

  val ROWS = 51

  def main(args: Array[String]) {
    val pascalNumbers = (
      for (r <- 0 until ROWS; c <- 0 until (r / 2 + 1)) yield
        fact(r) / fact(c) / fact(r - c)
      ).toSet
    println(pascalNumbers.flatMap(p => if (isSquareFree(p)) Some(p) else None).sum)
  }
}
