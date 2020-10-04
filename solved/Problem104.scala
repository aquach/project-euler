object Problem104 {

  def main(args: Array[String]) {
    var k = 2
    val iter = new Iterator[BigInt] {
      var old1 = BigInt(1)
      var old2 = BigInt(1)
      def hasNext = true
      def next = {
        k += 1
        var newNum = old1 + old2
        old1 = old2
        old2 = newNum
        newNum
      }
    }

    val MATCH = ('1' to '9').toList
    val MOD = BigInt("1000000000")
    val found = iter.find(i => {
      if ((i % MOD).toString.toList.sorted == MATCH) {
        println(k + " is a candidate")
        val str = i.toString
        if (str.length < 9) {
          false
        } else {
          str.take(9).toList.sorted == MATCH
        }
      } else {
        false
      }
    })
    println(k)
  }
}
