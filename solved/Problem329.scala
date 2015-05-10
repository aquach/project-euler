//P("R") = sum(P(croak "R" 1st && in pos i) for 1 <= i <= 500)
//P("RR") = sum(P(croak "R" first && in pos i first && croak "R" second && pos j after 1 hop) for 1 <= i <= 500, for j in [i - 1, i + 1 ])
//P("RR") = sum(P(croak "R" second && pos j after 1 hop | croak "R" first && in pos i first) P(croak "R" first && in pos i first) for 1 <= i <= 500, for j in [i - 1, i + 1 ])
//
//P(croak "R" 1st && in pos i first) = P(croak "R" 1st | in pos i) P(in pos i first)
//P(croak "R" 1st && in pos i first) = P(croak "R" 1st | in pos i) 1 / 500
//P(croak "R" 1st && in pos i first) = (if (i is prime) 2/3 else 1/3) 1 / 500
//
//P(croak "R" second && pos j after 1 hop | croak "R" first && in pos i first) =
//P(croak "R" second && pos j after 1 hop | in pos i first) =
//  hop chance = if (i == 1 || i == 500) 1 else 0.5
//  croak chance = if (j is prime) 2/3 else 1/3
//  hop chance * croak chance

object Problem329 {
  case class Rational(n: BigInt, d: BigInt) extends Ordered[Rational] {

    require(d != 0)

    private val g = gcd(n.abs, d.abs)
    val numer: BigInt = n / g
    val denom: BigInt = d / g

    def this(n: BigInt) = this(n, 1)

    override def toString = numer + "/" + denom

    def +(other: Rational): Rational =
      new Rational(
        this.numer * other.denom + other.numer * this.denom,
        this.denom * other.denom
      )

    def +(i: BigInt): Rational =
      new Rational(this.numer + i * this.denom, this.denom)

    def ++(): Rational =
      this + 1

    def -(other: Rational): Rational =
      new Rational(
        this.numer * other.denom - other.numer * this.denom,
        this.denom * other.denom
      )

    def -(i: BigInt): Rational =
      new Rational(this.numer + i * this.denom, this.denom)

    def --(): Rational =
      this - 1

    def *(other: Rational): Rational =
      new Rational(this.numer * other.numer, this.denom * other.denom)

    def *(i: BigInt): Rational =
      new Rational(this.numer * i, this.denom)

    def /(other: Rational): Rational =
      new Rational(this.numer * other.denom, this.denom * other.numer)

    def /(i: BigInt): Rational =
      new Rational(this.numer, this.denom * i)

    def max(other: Rational) =
      if(this.compareTo(other) > 0) this else other

    def min(other: Rational) =
      if(this.compareTo(other) < 0) this else other

    /* old -- replaced with Ordered[Rational] trait
    def compareTo(other: Rational): BigInt =
      (this.numer * other.denom) compareTo (other.numer * this.denom)
    def <(other: Rational) =
      this.numer * other.denom > other.numer * this.denom

    def >(other: Rational) = other < this
    def <=(other: Rational) = (this < other) || (this == other)
    def >=(other: Rational) = (this > other) || (this == other)
    */

   def compare(other: Rational) =
     ((this.numer * other.denom) - (other.numer * this.denom)).signum

   private def gcd(a: BigInt, b: BigInt): BigInt =
     if(b == 0) a else gcd(b, a % b)
  }

  val LIMIT = 500

  val sieve = {
    val SIEVE_LIMIT = LIMIT + 10
    val sieve = Array.fill(SIEVE_LIMIT)(true)
    sieve(0) = false
    sieve(1) = false

    Stream.from(2).takeWhile(i => i * i < SIEVE_LIMIT).filter(sieve(_)).foreach { i =>
      (i*i until SIEVE_LIMIT by i).foreach(sieve(_) = false)
    }

    Stream.from(1).takeWhile(_ < SIEVE_LIMIT).filter(n => sieve(n)).toSet
  }

  def chance(pInPos: Array[Rational], croak: Boolean) = {
    val pInNewPos = Array.fill(pInPos.length)(Rational(0, 1))

    pInPos.zipWithIndex.foreach({
      case (prior, i) => {
        val targets = List(i + 1, i - 1).filter(j => j >= 0 && j < LIMIT)

        targets.foreach(j => {
          val croakChance = if (croak ^ sieve(i + 1)) Rational(1, 3) else Rational(2, 3)
          val hopChance = Rational(1, targets.length)
          pInNewPos(j) += croakChance * hopChance * prior
        })
      }
    })

    pInNewPos
  }

  def main(x: Array[String]) {
    println(
      "PPPPNNPPPNPPNPN".map(_ == 'P')
        .foldLeft(Array.fill(LIMIT)(Rational(1, LIMIT)))((prior: Array[Rational], croakPrime: Boolean) =>
            chance(prior, croakPrime)
      ).reduce(_ + _)
    )
  }
}
