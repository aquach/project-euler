object Problem323 {

  val LIMIT = 1000
  def main(args: Array[String]) {
    val e = (0 to LIMIT).foldLeft(BigDecimal("0.0"))((s, i) =>
      s + 1 - (1 - BigDecimal("2").pow(-i)).pow(32)
    )
    println(f"$e%2.10f")
  }
}
