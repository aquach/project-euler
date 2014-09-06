object Problem110 {

  val LIMIT = 1000 * 1000

  def gcd(a: Int, b: Int): Int = {
    b match {
      case 0 => a
      case _ => gcd(b, (a % b))
    }
  }

  def lcm(a: Int, b: Int) = (a.toLong * b / gcd(a, b)).toInt

  def repunitReduction(k: Int, d: Int) = {
    val m = (d * (d % 10 match {
      case 1 => 9
      case 3 => 7
      case 7 => 3
      case 9 => 1
    }) + 1) / 10

    println(s"R($k) is divisible by $d if R(${k - 1}) + $m divides $d")
  }

  def findBaseWhereCheckingDividesDIsEasy(d: Int) = Iterator.from(1).map(exp => 
      (exp, BigInt(10).modPow(exp, d))
    ).find(x => x._2 == 1 || x._2 == (d - 1)).get

  def main(x: Array[String]) {
    println(Iterator.from(1000000).filter(n => gcd(n, 10) == 1).map(n => {
      val (digitGrouping, r) = findBaseWhereCheckingDividesDIsEasy(n)
      val factor = if (r == 1) {
        val oneGroupsResidue = (BigInt("1" * digitGrouping) % n).toInt
        if (oneGroupsResidue == 0)
          1
        else
          lcm(oneGroupsResidue, n) / oneGroupsResidue // Solves `digitGrouping * x === 0 mod n` for x.
      } else
        2 // Alternating sums of 1s, so you need two groups to cancel out and get 0.
      val k = digitGrouping * factor

      (n, k)
    }).find(_._2 >= LIMIT).get)
  }
}
