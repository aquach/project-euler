object Problem85 {

  def main(args: Array[String]) {
    val results = (for (w <- 1 to 100; h <- 1 to 100) yield {
      (w, h, (for (rw <- 1 to w; rh <- 1 to h) yield { (w - rw + 1) * (h - rh + 1) }).sum)
    })
    println(results.minBy({ case (_, _, rects) => math.abs(rects - 2000000) }))
  }
}
