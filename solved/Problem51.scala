object Problem51 {
  
  val LIMIT = 10000000
 
  def main(args: Array[String]) {
    val prime = Array.fill(LIMIT)(true)
    prime(0) = false
    prime(1) = false
  
    Stream.from(2).takeWhile(i => i*i < LIMIT).filter(prime(_)).foreach { i =>
      (i*i until LIMIT by i).foreach(prime(_) = false)
    }

    var seen = Set[Set[Int]]()

    for ((isPrime, num) <- prime.view.zipWithIndex if isPrime;
         config: Set[Int] <- (0 to num.toString.length).toSet.subsets) {

       val familyOfPrimes = (0 to 9).map({ digit =>
         config.foldLeft(num)({ (n, wild_pos) =>
           val pos = if (wild_pos == 0) 1 else List.fill(wild_pos)(10).reduce(_ * _)
           val d = (n / pos) % 10
           n - d * pos + digit * pos
         })
       }).toSet.filter(prime(_))

       if (familyOfPrimes.size == 8 && !seen.contains(familyOfPrimes) && familyOfPrimes.map(_.toString.length).toSet.size == 1) {
         seen = seen + familyOfPrimes
         println(familyOfPrimes.toList.sort(_ < _))
       }
    }
  }
}
