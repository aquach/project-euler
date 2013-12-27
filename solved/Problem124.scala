object Problem124 {

  def main(args: Array[String]) {
    val LIMIT = 100001
    val rad = Array.fill(LIMIT)(1)

    Stream.from(2).takeWhile(i => i < LIMIT).filter(rad(_) == 1).foreach { i =>
      (i until LIMIT by i).foreach(rad(_) *= i)
    }

    println((1 until LIMIT).map(x => (x, rad(x))).sortBy(_._2).apply(10000 - 1)._1)
  }
}
