object Problem101 {

  def l(oneX: Int, otherXes: Seq[Int])(targetX: Long): Long = {
    val numer = otherXes.map(targetX - _).reduce(_ * _)
    val denom = otherXes.map(oneX - _).reduce(_ * _)

    numer / denom
  }

  def fit(points: List[Long])(targetX: Long): Long = {
    if (points.size == 1)
      return points.head

    val xPoints = (1 to points.size)

    val polys = xPoints.zipWithIndex.map {
      case (x, index) => l(x, xPoints.patch(index, Nil, 1))(_)
    }

    polys.zip(points).map({
      case (poly, yValue) => poly(targetX) * yValue
    }).sum
  }

  def computeFITSum(sequence: Stream[Long]): Long = {
    Stream.from(1).map({ k =>
      val toFit = sequence.take(k).toList
      val next = sequence(k)

      val fittedPolynomial = fit(toFit)(_)
      val fittedSequence = Stream.from(1).map(_.toLong).map(fittedPolynomial)

      if (next == fittedSequence(k))
        0
      else
        fittedSequence(k)
    }).takeWhile(_ != 0).sum
  }

  def main(args: Array[String]) {
    def f(x: Long) = 1 + (1 to 10).map(pow => List.fill(pow)(x).reduce(_ * _) * (if (pow % 2 == 0) 1 else -1)).sum

    val sequence = Stream.from(1).map(_.toLong).map(f)

    println(computeFITSum(sequence))
  }
}
