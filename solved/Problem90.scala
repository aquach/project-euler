object Problem90 {

  val squares = (1 to 9).map(x => x * x).map(x => (x / 10, x % 10))

  def valid(c1: List[Int], c2: List[Int]) = {
    val (s1, s2) = (c1.toSet, c2.toSet)

    squares.forall { sq =>
      val (d1, d2) = sq

      (for (
        (cube1, cube2) <- List((s1, s2), (s2, s1)).toIterator;
        digit1 <- if (d1 == 6 || d1 == 9) List(6, 9) else List(d1);
        digit2 <- if (d2 == 6 || d2 == 9) List(6, 9) else List(d2)
      ) yield {
        cube1.contains(digit1) && cube2.contains(digit2)
      }).exists(identity)
    }
  }

  def main(args: Array[String]) {
    val digits = (0 to 9).toList
    println((for (
      cube1 <- digits.combinations(6);
      cube2 <- digits.combinations(6) if valid(cube1, cube2)
    ) yield {
      Set(cube1, cube2)
    }).toSet.size)
  }
}
