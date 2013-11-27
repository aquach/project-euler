object Project145 {
  def main(args: Array[String]) {
    println(
      (0 to 1000 * 1000 * 1000).par
        .filter(i => i % 10 != 0 &&
           (i + i.toString.reverse.toInt).toString.forall(
             j => j == '1' || j == '3' || j == '5' || j == '7' || j == '9'
           )
        )
        .size
     )
  }
}
