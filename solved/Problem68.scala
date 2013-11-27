object Problem68 {

  def isValid(a: IndexedSeq[Int]): Boolean = {
    val sum = a(0) + a(6) + a(7)
    (a(1) + a(7) + a(8)) == sum &&
    (a(2) + a(8) + a(9)) == sum &&
    (a(3) + a(9) + a(5)) == sum &&
    (a(4) + a(5) + a(6)) == sum
  }

  def solutionToBigInt(a: IndexedSeq[Int]): BigInt = {
    val solutionList = Array(
      Array(a(0), a(6), a(7)),
      Array(a(1), a(7), a(8)),
      Array(a(2), a(8), a(9)),
      Array(a(3), a(9), a(5)),
      Array(a(4), a(5), a(6))
    )
    val minIndex = solutionList.zipWithIndex.minBy(_._1(0))._2
    val reordered = solutionList.slice(minIndex, solutionList.length) ++ solutionList.slice(0, minIndex)
    BigInt(reordered.flatten.mkString)
  }

  def main(args: Array[String]) {
    println(
      (1 to 10).permutations.filter(x => isValid(x) && solutionToBigInt(x).toString.length == 16).map(solutionToBigInt(_)).max
    )
  }
}
