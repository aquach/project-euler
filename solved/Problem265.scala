def nonrepeats(s: String, max: Int, window: Int): Boolean = {
  val circle = if (s.length == max) (s + s.take(window - 1)) else s
  val r = circle.sliding(window).map(_.toList).toList
  r.toSet.size == r.length
}

def rsolve(path: String, window: Int, max: Int): Set[String] = {
  if (path.length == max) {
    return Set(path)
  }

  val s0 = path + "0"
  val s1 = path + "1"
  (if (nonrepeats(s0, max, window)) rsolve(s0, window, max) else Set()) ++
  (if (nonrepeats(s1, max, window)) rsolve(s1, window, max) else Set())
}

def solve(n: Int) = {
  val start = "0" * n
  val arrangements = rsolve(start, n, BigInt(2).pow(n).toInt)
  arrangements.map(a => Integer.parseInt(a, 2)).map(BigInt(_)).sum
}

println(solve(5))
