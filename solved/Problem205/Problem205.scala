object Problem205 {

  def loadDistribution(filename: String): Map[Int, Int] = {
    val source = scala.io.Source.fromFile(filename)
    val lines = source.mkString
    source.close()

    Map(lines.split("\n").map(_.trim.split(" ") match { case Array(probability, number) => (number.toInt, probability.toInt) }): _*)
  }

  def main(args: Array[String]) {
    var peter = loadDistribution("nine-rolls-of-four.txt")
    var colin = loadDistribution("six-rolls-of-six.txt")

    val peterTotal = peter.values.sum
    val colinTotal = colin.values.sum

    println((for (pRoll <- peter; cRoll <- colin) yield {
      if (pRoll._1 > cRoll._1) {
        (pRoll._2 * cRoll._2).toLong
      } else {
        0
      }
    }).sum.toDouble / peterTotal / colinTotal)
  }
}
