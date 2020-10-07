val LIMIT = 17

println("Computing triples...")
val LIMIT_SQRT = Math.sqrt(LIMIT).toInt


val ineligibleBuf = new scala.collection.mutable.ArrayBuffer[(Int, Int)](5000)

var m = 1

while (m <= LIMIT_SQRT) {
  var n = 1
  while (n < m) {
    val a = m * m - n * n
    val b = 2 * m * n
    if (a <= LIMIT && b <= LIMIT) {
      ineligibleBuf += ((a * a, b * b))
    }
    n += 1
  }

  m += 1
}

val ineligiblePoints = ineligibleBuf.toSet
// val ineligiblePoints = Set[(Int, Int)]()

println(ineligiblePoints)
println("Done.")

var currentRow = Array.fill(LIMIT)(BigInt(1))

var n = 1

while (n < LIMIT) {
  println(n)
  println(currentRow.toList)

  for (i <- 1.until(currentRow.length)) {
    if (!ineligiblePoints((n, i)) && !ineligiblePoints((i, n))) {
      currentRow(i) = currentRow(i - 1) + currentRow(i)
    } else {
      currentRow(i) = 0
    }
  }

  n += 1
}

println(currentRow.toList)
