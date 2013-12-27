object Problem199 {
  import math.sqrt

  val R = 1.0
  val r = sqrt(3) / (2 + sqrt(3)) * R

  val ITERATIONS = 10

  def descartes(k1: Double, k2: Double, k3: Double): (Double, Double) = {
    val s = 2 * sqrt(k1 * k2 + k2 * k3 + k3 * k1)
    val c = k1 + k2 + k3
    (s + c, s - c)
  }

  def sumRSq(curvatures: (Double, Double, Double), iter: Int): Double = {
    if (iter == ITERATIONS)
      return 0

    val newCurvature = Function.tupled(descartes _)(curvatures)._1

    1 / (newCurvature * newCurvature) +
      sumRSq((curvatures._1, curvatures._2, newCurvature), iter + 1) +
      sumRSq((curvatures._2, curvatures._3, newCurvature), iter + 1) +
      sumRSq((curvatures._1, curvatures._3, newCurvature), iter + 1)
  }

  def main(args: Array[String]) {
    val insideCurvatures = (1 / r, 1 / r, 1 / r)
    val outsideCurvatures = (-1 / R, 1 / r, 1 / r)

    println(1 - (r * r * 3 + sumRSq(insideCurvatures, 0) + sumRSq(outsideCurvatures, 0) * 3) / (R * R))
  }
}
