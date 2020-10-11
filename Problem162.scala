def crossJoin[T](list: List[List[T]]): List[List[T]] =
  list match {
    case xs :: Nil => xs map (List(_))
    case x :: xs => for {
      i <- x
      j <- crossJoin(xs)
    } yield List(i) ++ j
  }

def fact(i: Int): Long = {
  if (i <= 1)
    return 1
  1.to(i).foldLeft(1L)(_ * _)
}

def solve(n: Int): Long = {
  3.to(n).map { numDigits =>
    val freeDigits = numDigits - 3

    val numPerms = fact(numDigits) / fact(freeDigits) * 2 / 3

    println(numDigits, freeDigits, numPerms)

    numPerms * Iterator.fill(freeDigits)(16L).foldLeft(1L)(_ * _)
  }.sum
}

def slowsolve(n: Int): Int = {
  val results = crossJoin(
    Array.fill(n)(0.to(15).map(_.toHexString).toList).toList
  )
    .toIterator
    .map { ds => Integer.parseInt(ds.mkString, 16).toHexString }
    .toSet
    .filter { s =>
      s.contains("a") && s.contains("1") && s.contains("0")
    }

    println(results.toList.groupBy(_.head).mapValues(_.length).toMap)
    results.size
}

println(solve(2))
println(slowsolve(2))
println()
println(solve(3))
println(slowsolve(3))
println()
println(solve(4))
println(slowsolve(4))
println()
println(solve(5))
println(slowsolve(5))
