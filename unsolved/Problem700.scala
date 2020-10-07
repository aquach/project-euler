val base = BigInt("1504170715041707")
val mod = BigInt("4503599627370517")

var minCoin = base
var sum = base
var n = 1

println(n, minCoin)

while (n < 30000) {
  val newCoin = (base * n).mod(mod)

  if (newCoin < minCoin) {
    minCoin = newCoin
    println(n, minCoin)
    sum += newCoin
  }

  n += 1
}

(1.to(2600)).foreach { v =>
  println("%10d %d".format(v, (base * v).mod(mod)))
}


println((mod - base * 2).mod(mod))
println((base * 3).mod(mod))
println((mod - base * 2).mod(mod) / (base * 3).mod(mod))

// println("1", (base * 1).mod(mod))
// println("2", (base * 2).mod(mod))
// println("3", (base * 3).mod(mod))
// println("4", (base * 4).mod(mod))
// println("5", (base * 5).mod(mod))
// println("6", (base * 6).mod(mod))
// println("7", (base * 7).mod(mod))
// println("8", (base * 8).mod(mod))
// println("9", (base * 9).mod(mod))

// println("11", (base * 11).mod(mod))
// println("46", (base * 46).mod(mod))
// println("253", (base * 253).mod(mod))
// println("506", (base * 506).mod(mod))

// // println("diff", (base * 506).mod(mod) - (base * 1014).mod(mod))

// // println(mod - (base * 503).mod(mod))
// // val stride = ((base * 3).mod(mod) + (base * 503).mod(mod)).mod(mod)
// // println(stride)

// // println(((base * 506).mod(mod) * 5).mod(mod))
