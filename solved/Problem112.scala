object Problem112 {

  def bouncy(x: Int): Boolean = {
    val str = x.toString
    val pairs = str.zip(str.tail)
    x >= 100 &&
    !pairs.forall({ case (x, y) => x <= y }) &&
    !pairs.forall({ case (x, y) => x >= y })
  }

  def main(args: Array[String]) {
    var i = 0
    var numBouncy = 0

    while (i == 0 || numBouncy * 100 / i < 99) {
      i += 1
      if (bouncy(i))
        numBouncy += 1
    }

    println(i)
  }
}
