object Problem103 {
  def subsets[T](s: List[T]): List[(List[T], List[T])] = s match {
    case List() => List((List(), List()))
    case x :: xs => {
      val restPartitions = subsets(xs)
      restPartitions.map(p => (p._1, x :: p._2)) ++
        restPartitions.map(p => (x :: p._1, p._2)) ++
        restPartitions
    }
  }

  def isSpecialSum(s: List[Int]) = subsets(s).forall({
      case (List(), List()) => true
      case (b, c) => b.sum != c.sum && (if (b.size > c.size) b.sum > c.sum else true)
  })

  val LIMIT = 2

  def variations(s: List[Int]): List[List[Int]] = s match {
    case List() => List(List())
    case x :: xs => {
      val vs = variations(xs)
      for (a <- (-LIMIT to LIMIT).toList; v <- vs) yield (x + a) :: v
    }
  }

  def main(args: Array[String]) {
    val candidate = List(20, 31, 38, 39, 40, 42, 45)

    println(
      variations(candidate)
      .map(_.toSet)
      .filter(_.size == 7)
      .filter(s => isSpecialSum(s.toList))
      .minBy(_.sum)
      .toList
      .sorted
    )
  }
}
