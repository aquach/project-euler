val str = """
5616185650518293 ;2 correct
3847439647293047 ;1 correct
5855462940810587 ;3 correct
9742855507068353 ;3 correct
4296849643607543 ;3 correct
3174248439465858 ;1 correct
4513559094146117 ;2 correct
7890971548908067 ;3 correct
8157356344118483 ;1 correct
2615250744386899 ;2 correct
8690095851526254 ;3 correct
6375711915077050 ;1 correct
6913859173121360 ;1 correct
6442889055042768 ;2 correct
2321386104303845 ;0 correct
2326509471271448 ;2 correct
5251583379644322 ;2 correct
1748270476758276 ;3 correct
4895722652190306 ;1 correct
3041631117224635 ;3 correct
1841236454324589 ;3 correct
2659862637316867 ;2 correct
"""

case class Guess(digits: List[Int], numCorrect: Int)

val allGuesses = str.split("\n").map(_.split(" ")).collect {
  case Array(code, numCorrectStr, _) =>
    Guess(code.split("").map(_.toInt).toList, numCorrectStr.drop(1).toInt)
}.toList.sortBy(_.numCorrect)

val LENGTH = allGuesses.head.digits.length

@inline
def p2b(pos: Int, digit: Int) = pos * 10 + digit

@inline
def b2p(b: Int) = (b / 10, b % 10)

@inline
def availableDigits(b: scala.collection.BitSet, pos: Int) = p2b(pos, 0).to(p2b(pos, 9)).filterNot(b).map(b => b2p(b)._2)

@inline
def isValid(b: scala.collection.BitSet, pos: Int) = !p2b(pos, 0).to(p2b(pos, 9)).forall(b)

def solve(guesses: List[Guess], ruledOutBits: scala.collection.BitSet): Option[String] = guesses match {
  case Nil =>
    val digits = 0.until(LENGTH).map(p => availableDigits(ruledOutBits, p))
    if (digits.forall(ds => ds.size == 1)) {
      Some(digits.map(_.head).mkString(""))
    } else if (digits.exists(ds => ds.size > 1)) {
      throw new Exception("Non-unique solution. Should be impossible?")
    } else {
      None
    }

  case g :: gs =>
    val ds = g.digits

    ds.zipWithIndex.combinations(g.numCorrect).toIterator.flatMap { choices =>
      if (guesses.length >= 15) {
        println(s"${"  " * (22 - guesses.length)} $g $choices")
      }

      val bits = scala.collection.mutable.BitSet.fromBitMask(ruledOutBits.toBitMask)

      val chosenIndices = choices.map(_._2)

      0.until(LENGTH).foreach { i =>
        if (!chosenIndices.contains(i))
          bits += p2b(i, ds(i))
      }

      choices.foreach { case (digit, p) =>
        0.to(9).filter(d => d != digit).foreach { d =>
          bits += p2b(p, d)
        }
      }

      if (0.until(LENGTH).forall(p => isValid(bits, p))) {
        solve(gs, bits)
      } else {
        None
      }
    }.nextOption
}

println(solve(allGuesses, scala.collection.BitSet.empty))
