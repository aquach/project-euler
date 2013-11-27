object Problem61 {

  def isChain(c: List[Int])(predicate: (Int, Int) => Boolean) = 
    !c.isEmpty && predicate(c.last, c.head) && c.zip(c.tail).forall(Function.tupled(predicate))

  def matches(a: Int, b: Int) = b.toString.startsWith(a.toString.takeRight(2))

  def findChain(existingChain: List[Int], lists: Set[List[Int]]): Option[List[Int]] =
    (lists.size, isChain(existingChain)(matches(_, _))) match {
      case (l, true) if l == 0 => Some(existingChain)
      case (l, false) if l == 0 => None
      case (_, _) => 
        (for (seqList <- lists.iterator;
             candidate <- seqList.iterator.filter(existingChain.isEmpty || matches(existingChain.last, _))
        ) yield {
          findChain(existingChain :+ candidate, lists - seqList)
        }).find(_.isDefined).getOrElse(None)
    }

  def main(args: Array[String]) {
    val lists = Seq(
      (n: Int) => n * (n + 1) / 2,
      (n: Int) => n * n,
      (n: Int) => n * (3 * n - 1) / 2,
      (n: Int) => n * (2 * n - 1),
      (n: Int) => n * (5 * n - 3) / 2,
      (n: Int) => n * (3 * n - 2)
    ).map(seqFn => Stream.from(1).map(seqFn).dropWhile(_ < 1000).takeWhile(_ <= 9999).toList).toSet

    println(findChain(List(), lists).map(_.sum))
  }
}
