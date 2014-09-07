object Problem109 {

  def score(checkout: List[(Int, Int)]) = checkout.map(t => t._1 * t._2).sum

  def main(x: Array[String]) {
    val finishDarts = 
      for (
        finishScore <- 25 :: (1 to 20).toList
      ) yield (2, finishScore)

    val allDarts =
      for (
        score <- 25 :: (1 to 20).toList;
        mult <- List(1, 2) ++ (if (score == 25) Nil else List(3))
      ) yield (mult, score)

    val ways =
      (
        finishDarts.map(d => List(d)) ++
        (for (dart1 <- allDarts; dart2 <- finishDarts) yield List(dart1, dart2)) ++
        (for (dart1 <- allDarts; dart2 <- allDarts; dart3 <- finishDarts) yield List(dart1, dart2).sorted :+ dart3)
      ).toSet

    println(ways.filter(score(_) < 100).size)
  }
}
