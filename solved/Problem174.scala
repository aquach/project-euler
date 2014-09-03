object Problem174 {

  val LIMIT = 1000 * 1000

  def numDistinctLamina(numTiles: Int) = 
    (2 to math.sqrt(numTiles).toInt).count(d =>
      if (numTiles % d == 0) {
        val xPlusH = numTiles / d
        val xMinusH = d
        if ((xPlusH - xMinusH) % 2 == 0 && (xPlusH + xMinusH) % 2 == 0) {
          val h = (xPlusH - xMinusH) / 2
          val x = (xPlusH + xMinusH) / 2
          ((h - x) % 2 == 0) && h != 0 && x != 0
        } else false
      }
      else false
    )

  def main(args: Array[String]) {
    val frequenciesByDistinctLamina = (0 to LIMIT by 4).map(numDistinctLamina).groupBy(identity).mapValues(_.size)
    println((1 to 10).map(frequenciesByDistinctLamina).sum)
  }
}
