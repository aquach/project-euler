object Problem125 {

  val LIMIT = 100000
  val N_LIMIT = 100000000

  def main(args: Array[String]) {
    val runningSquares = (1 until LIMIT).iterator.map(x => BigInt(x) * x).scanLeft(BigInt(0))(_ + _).toArray

    println(
      (
        for (start <- (0 until LIMIT).iterator;
             square <- (start + 2 until LIMIT).iterator
                .map(runningSquares(_) - runningSquares(start))
                .takeWhile(_ < N_LIMIT);
             str = square.toString if str == str.reverse
            ) yield {
          square
        }
      ).toSet.sum
    )
  }
}
