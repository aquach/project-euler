object Problem106 {

  def subsets[T](s: List[T]): List[(List[T], List[T])] = s match {
    case List() => List((List(), List()))
    case x :: xs => {
      val restPartitions = subsets(xs)
      restPartitions.map(p => (p._1, x :: p._2)) ++
        restPartitions.map(p => (x :: p._1, p._2)) ++
        restPartitions
    }
  }

  val LIMIT = 12

  def main(args: Array[String]) {
    val s = subsets((0 until LIMIT).toList)
    println(
      s.filter(x => x._1.size == LIMIT / 2 && x._2.size == LIMIT / 2)
        .filterNot(x => {
          val zipped = x._1.toList.sorted.zip(x._2.toList.sorted)
          zipped.forall(n => n._1 < n._2) || zipped.forall(n => n._1 > n._2)
        })
       .map(x => Set(x._1.toSet, x._2.toSet)).toSet
    )
  }
}
