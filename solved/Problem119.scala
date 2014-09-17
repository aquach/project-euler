object Problem119 {
  def digitSum(b: BigInt) = b.toString.map(_ - '0').sum

  def main(x: Array[String]) {
    val results = 
      for (b <- 0 to 1000; e <- 0 to 10; p = BigInt(b).pow(e) if p > 10 && b == digitSum(p))
        yield p

    println(results.sorted.apply(29))
  }
}
