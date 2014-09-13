object Problem121 {
  def twoPartitions[T](s: List[T]): List[(List[T], List[T])] = s match {
    case List() => List((List(), List()))
    case x :: xs => {
      val restPartitions = twoPartitions(xs)
      restPartitions.map(p => (p._1, x :: p._2)) ++ restPartitions.map(p => (x :: p._1, p._2))
    }
  }

  val NUM_TURNS = 15

  def main(args: Array[String]) {
    val outcomes = twoPartitions((0 until NUM_TURNS).toList)
    val winningOutcomes = outcomes.filter(s => s._1.size > s._2.size)
    val winningChance = winningOutcomes.map({ case (bluePicks, redPicks) => 
      (bluePicks.map(p => 1.0 / (2 + p)) ++ redPicks.map(p => 1 - 1.0 / (2 + p))).reduce(_ * _)
    }).sum
    println((1 / winningChance).toInt)
  }
}
