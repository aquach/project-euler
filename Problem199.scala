object Problem88 {
  import math.sqrt

  val R = 1.0
  val r = sqrt(3) / (2 + sqrt(3)) * R

  val ITERATIONS = 3

  def descartes(r1: Double, r2: Double, r3: Double): (Double, Double) = {
    val (k1, k2, k3) = (1 / r1, 1 / r2, 1 / r3)
    val s = 2 * sqrt(k1 * k2 + k2 * k3 + k3 * k1)
    val c = k1 + k2 + k3
    (1 / (s + c), 1 / (s - c))
  }


  def sumRSq(radii: (Double, Double, Double), iter: Int): Double = {
    if (iter == ITERATIONS)
      return 0

    val newRadius = Function.tupled(descartes _)(radii)._1
    println(radii, newRadius)

    newRadius * newRadius +
      sumRSq((radii._1, radii._2, newRadius), iter + 1) +
      sumRSq((radii._2, radii._3, newRadius), iter + 1) +
      sumRSq((radii._1, radii._3, newRadius), iter + 1)
  }

  def main(args: Array[String]) {
    val insideCircles = (r, r, r)
    val outsideCircles = (R, r, r)

    //println(1 - (r * r * 3 + sumRSq(insideCircles, 0) + sumRSq(outsideCircles, 0) * 3) / (R * R))
  }
}
