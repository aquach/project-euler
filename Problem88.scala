object Problem88 {
  def getAllProductMaps(x: Int, k: Int): Set[Map[Int, Int]] = 
    if (k == 1) {
      Set[Map[Int, Int]](Map(x -> 1))
    } else {
      (for (i <- (1 to x / 2).filter(x % _ == 0);
           productMap <- getAllProductMaps(x / i, k - 1)
         ) yield {
         productMap.updated(i, productMap.get(i).getOrElse(0) + 1)
       }).toSet
    }

  def isProductSum(x: Int, k: Int) =
    !getAllProductMaps(x, k).filter(_.toIterator.map(Function.tupled(_ * _)).sum == x).isEmpty

  def main(args: Array[String]) {
    println(
      (for (
        k <- (2 to 50).toIterator;
        min = Stream.from(k).toIterator.dropWhile(i => !isProductSum(i, k)).next
      ) yield {
        println(k, min, getAllProductMaps(min, k).filter(_.toIterator.map(Function.tupled(_ * _)).sum == min))
        min
      }
     ).toSet.sum
    )
  }
}
