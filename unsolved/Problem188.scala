object Problem188 {

  def main(args: Array[String]) {
    var a = BigInt(1777)
    (0 until 1855).foreach({ i =>
    println(i)
      a = BigInt(a.pow(1777).toString.takeRight(10))
    })

    println(a)
  }
}
