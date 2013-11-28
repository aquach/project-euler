object Problem91 {

  val LIMIT = 50

  def main(args: Array[String]) {
    println(
      (for (
        x1 <- (0 to LIMIT).iterator;
        y1 <- (0 to LIMIT).iterator if !(x1 == 0 && y1 == 0);
        x2 <- (0 to LIMIT).iterator;
        y2 <- (0 to LIMIT).iterator if !(x1 == x2 && y1 == y2) && !(x2 == 0 && y2 == 0);
        sides = Vector(
          y1 * y1 + x1 * x1,
          x2 * x2 + y2 * y2,
          (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
        ).sortBy(identity) if (sides(0) + sides(1) == sides(2))
      ) yield {
        1
      }).size / 2
   )
  }
}
