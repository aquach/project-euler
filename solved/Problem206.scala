object Problem206 {
  
  def main(args: Array[String]) {
    val it = new Iterator[BigInt] {
      var i = BigInt(10000003)
      var j = 0
      def hasNext = true
      def next(): BigInt = {
        i += (if (j % 2 == 0) 4 else 6)
        j += 1
        i
      }
    }

    val answer = it.find(i => {
        val square = (i * i).toString
        square.length == 17 &&
        square(16) == '9' &&
        square(14) == '8' &&
        square(12) == '7' &&
        square(0) == '1' &&
        square(2) == '2' &&
        square(4) == '3' &&
        square(6) == '4' &&
        square(8) == '5' &&
        square(10) == '6'
      }).get

    println(answer)
    println(answer * answer)
  }
}
