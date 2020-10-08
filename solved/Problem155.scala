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

val one = Rational(1, 1)

def d(n: Int): Int = {
  val rationalsByCaps = scala.collection.mutable.HashMap.empty[Int, Set[Rational]]
  rationalsByCaps.put(1, Set(one))

  2.to(n).foreach { totalNeeded =>
    println(totalNeeded)

    val adds = scala.collection.mutable.HashSet.empty[Rational]

    1.to(totalNeeded / 2).foreach { a =>
      val as = rationalsByCaps.get(a).getOrElse(Set())
      val bs = rationalsByCaps.get(totalNeeded - a).getOrElse(Set())

      as.foreach { av =>
        bs.foreach { bv =>
            adds += av + bv
            adds += Rational(
              av.numer * bv.numer,
              av.numer * bv.denom + av.denom * bv.numer
            )
            // adds += one / (one / av + one / bv)
        }
      }
    }

    rationalsByCaps.put(totalNeeded, adds.toSet)
  }

  rationalsByCaps.values.flatten.map(_ * 60).toList.toSet.size
}

println(d(18))

trait Node
object Single extends Node {
  override def toString = "Node"
}
case class Serial(a: Node, b: Node) extends Node
case class Parallel(a: Node, b: Node) extends Node

def choices(n: Node): Set[Node] = n match {
  case Single =>
    Set(Serial(Single, Single), Parallel(Single, Single))
  case Serial(a, b) =>
    choices(a).map(x => Serial(x, b)) ++ choices(b).map(x => Serial(a, x))
  case Parallel(a, b) =>
    choices(a).map(x => Parallel(x, b)) ++ choices(b).map(x => Parallel(a, x))
}

def cap(n: Node): Rational = n match {
  case Single => one
  case Serial(a, b) => one / (one / cap(a) + one / cap(b))
  case Parallel(a, b) => cap(a) + cap(b)
}

def e(n: Int): Set[Node] = {
  1.until(n).foldLeft[Set[Node]](Set(Single)) { (existingCombos, _) =>
    existingCombos ++ existingCombos.flatMap { n => choices(n) }
  }
}

// println(e(6).map(cap).map(_ * 60).toList.sorted)
// println(e(6).map(cap).toList.sorted)
// println(e(6).map(cap).map(_ * 60).toList.sorted.length)
// println(e(6).map(cap).map(_ * 1).toList.sorted.length)
// println(e(6).map(cap).toList.sorted.length)
// println(e(6).map(cap).toSet.size)

// println(e(3).map(cap).size)
// println(e(4).map(cap).size)
// println(e(5).map(cap).size)
// println(e(5).map(cap).map(_ * 60).size)
// println(e(5).map(cap).map(_ * 60).toList.sorted)
