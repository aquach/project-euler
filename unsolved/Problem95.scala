object Problem95 {

  val LIMIT = 100

  def main(args: Array[String]) {
    val divisors = Array.fill(LIMIT)(0)

    for (n <- 1 until LIMIT; j <- n * 2 until LIMIT by n) {
      divisors(j) += n
    }

    val result = (0 until divisors.length).foldLeft(Map[Int, Int]())((map, number) => {
      val explorer = Iterator.iterate((Set[Int](), Some(number).asInstanceOf[Option[Int]]))({
        case (seen, Some(i)) => {
          val next = divisors(i)
          if (seen(next) || next >= LIMIT)
            (seen, None)
          else
            (seen + next, Some(next))
        }
        case (seen, None) => (seen, None)
      })

      val chain = explorer.find(!_._2.isDefined).get._1.toList.sorted
      println(number, chain)
      val first = chain.head
      map ++ chain.map(i => (i, first)).toMap
    })

    println(result)
  }
}
