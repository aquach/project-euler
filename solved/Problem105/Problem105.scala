import scala.io.Source

object Problem105 {

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

  def main(args: Array[String]) {
    println(
      Source.fromFile("p105_sets.txt").getLines
      .map(_.split(",").map(_.toInt).toList)
      .filter(isSpecialSum)
      .map(_.sum)
      .sum
    )
  }
}
