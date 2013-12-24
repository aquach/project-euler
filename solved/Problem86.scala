object Problem86 {
  val squares = (0 to 5000).map(x => x * x).toSet

  def shortestRouteIsInteger(l: Int, w: Int, h: Int): Boolean = {
    val r = Seq(
      (l * l + (w + h) * (w + h)),
      (w * w + (l + h) * (l + h)),
      (h * h + (l + w) * (l + w))
    ).min

    squares.contains(r)
  }

  def main(args: Array[String]) {
    val M = 1818
    println((for (
      l <- (1 to M).par;
      w <- (l to M).par;
      h <- w to M if shortestRouteIsInteger(l, w, h)
    ) yield {
      1
    }).size)
  }
}
