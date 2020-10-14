case class Rational(numer: BigInt, denom: BigInt) extends Ordered[Rational] {
  require(denom != 0)

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

 override def equals(other: Any): Boolean = other match {
   case r: Rational => compare(r) == 0
   case _ => false
 }

 lazy val ga = gcd(numer, denom)

 override def hashCode(): Int = (numer / ga, denom / ga).hashCode

 def compare(other: Rational) = {
   val newN = this.numer * other.denom - other.numer * this.denom
   val newD = this.denom * other.denom
   (newN * newD).signum
 }

 def reduce(): Rational = {
   val g = gcd(numer, denom)
   Rational(numer / g, denom / g)
 }

 def toDouble: Double = {
   (BigDecimal(numer) / BigDecimal(denom)).toDouble
 }

 private def gcd(a: BigInt, b: BigInt): BigInt =
   if(b == 0) a else gcd(b, a % b)
}
