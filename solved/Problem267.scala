case class Memo[A, B](f: A => B) extends (A => B) {
  private val cache = scala.collection.mutable.Map.empty[A, B]
  def apply(x: A) = cache.getOrElseUpdate(x, f(x))
}

val fact: Memo[Int, BigInt] = Memo {
  case 0 => 1
  case n => n * fact(n - 1)
}

def winnings(heads: Int, f: Double): Double = {
  Math.pow(1 + 2 * f, heads) * Math.pow(1 - f, 1000 - heads)
}

val expN = Math.pow(0.5, 1000)
def binomialP(heads: Int): Double = {
  val result = fact(1000) / fact(heads) / fact(1000 - heads)
  (expN * BigDecimal(result)).toDouble
}

val binomials = 0.to(1000).map(binomialP).toArray

def chanceOverBillion(f: Double): Double = {
  0.to(1000).map { heads =>
    if (winnings(heads, f) >= 1000 * 1000 * 1000) {
      binomials(heads)
    } else {
      0
    }
  }.sum
}

var LR = 0.01

var guess = 0.1

for (_ <- 0.to(10000)) {
  val c = chanceOverBillion(guess)
  println(guess, c)

  val a = chanceOverBillion(guess - LR)
  val b = chanceOverBillion(guess + LR)

  if (a > c) {
    guess -= LR
  } else if (b > c) {
    guess += LR
  } else {
    println("halving learning rate")
    LR *= 0.5
  }
}

println(chanceOverBillion(guess))
