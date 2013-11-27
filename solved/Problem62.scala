object Problem62 {
  
  val LIMIT = 100000

  def main(args: Array[String]) {
    val cubes = (0 to LIMIT).map(x => x.toLong * x * x)
    val anagrams = cubes.groupBy(_.toString.toList.sort(_ < _))

    println(anagrams.filter({ case (k, v) => v.length == 5 }).values.flatten.min)
  }
}
