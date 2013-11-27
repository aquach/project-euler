object Problem93 {

  class Rational(val n: Int, val d: Int) {
    def +(that: Rational) = new Rational(n * that.d + that.n * d, d * that.d)
    def -(that: Rational) = new Rational(n * that.d - that.n * d, d * that.d)
    def *(that: Rational) = new Rational(n * that.n, d * that.d)
    def /(that: Rational) = new Rational(n * that.d, d * that.n)

    def toInt: Option[Int] = if (d != 0 && n % d == 0) Some(n / d) else None

    override def toString = n + "/" + d
  }

  implicit def intToRational(x: Int): Rational = new Rational(x, 1)

  sealed trait Token
  case class Value(d: Rational) extends Token
  sealed trait Operator extends Token {
    def apply(a: Rational, b: Rational): Rational
  }
  case object Plus extends Operator {
    def apply(a: Rational, b: Rational) = a + b
  }
  case object Minus extends Operator {
    def apply(a: Rational, b: Rational) = a - b
  }
  case object Times extends Operator {
    def apply(a: Rational, b: Rational) = a * b
  }
  case object Divides extends Operator {
    def apply(a: Rational, b: Rational) = a / b
  }

  def evaluate(seq: Seq[Token]): Option[Rational] = try {
    seq.foldLeft(List[Rational]())((stack, nextToken) => (stack, nextToken) match {
      case (a :: b :: xs, o: Operator) => o.apply(a, b) :: xs
      case (xs, v: Value) => v.d :: xs
    }) match {
      case x :: Nil => Some(x)
      case _ => None
    }
  } catch {
    case e: MatchError => None
  }

  type Stack = List[Rational]

  def search(seq: List[Value], stack: Stack): List[Stack] = (stack match {
    case a :: b :: xs => List(Plus, Minus, Times, Divides).flatMap(op => search(seq, op.apply(a, b) :: xs))
    case stack @ x :: Nil if seq.isEmpty => List(stack)
    case _ => Nil
  }) ++ (seq match {
    case x :: xs => search(xs, x.d :: stack)
    case Nil => Nil
  })

  def search(seq: List[Value]): List[Stack] = search(seq, Nil)

  def main(args: Array[String]) {
    println(
      (0 to 9).combinations(4).map(perm => 
        (perm, perm.permutations.flatMap(perm => search(perm.map(Value(_)).toList).map(_.head)).flatMap(_.toInt).filter(_ > 0).toSet.toList.sortWith(_ < _).zipWithIndex.takeWhile({ case (x, y) => x - 1 == y }).length)
      ).maxBy(_._2)
    )
  }
}
