object Problem122 {

  def main(args: Array[String]) {
    lazy val stream: Stream[Set[Int]] = Set[Int]() #:: Set[Int]() #:: {
      def computeMinMults(n: Int): Stream[Set[Int]] =
        (
          (for (r <- (1 until n).iterator) yield { 
              println(n, r, stream(r), stream(n - r))
            (stream(r) ++ stream(n - r)) ++ Set(r, n - r)
          }).minBy(_.size)
        ) #:: computeMinMults(n + 1)
      computeMinMults(2)
    }
    println(stream.take(16).zipWithIndex.force)
  }
}
