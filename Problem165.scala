import $file.utils
import utils.Rational

case class Point(x: Int, y: Int)
case class Line(p1: Point, p2: Point)

val TEST = Array(27, 44, 12, 32, 46, 53, 17, 62, 46, 70, 22, 40)

val generator = Iterator.iterate(290797L)(s => s * s % 50515093).drop(1)

val points = generator.map(_ % 500).map(_.toInt).take(20000).toArray
val pts = points.grouped(2).map { case Array(x, y) => Point(x, y) }.toList
val allLines = pts.grouped(2).map { case Seq(p1, p2) => Line(p1, p2) }.toList
if (allLines.distinct.size != allLines.size) {
  println(allLines.groupBy(identity).filter { case (_, v) => v.length > 1 })
  throw new Exception("AHH")
}

val allLinesSet = allLines.toSet

val zero = Rational(0, 1)
val one = Rational(1, 1)

def solve(lines: Set[Line]): scala.collection.mutable.Set[(Rational, Rational)] = {
  println("Drawing grid...")
  val maxX = pts.map(_.x).max
  val maxY = pts.map(_.y).max

  val grid = Array.fill(maxX + 1, maxY + 1)(scala.collection.mutable.Set.empty[Line])

  lines.foreach {
    case l@Line(a, b) =>
      if (Math.abs(a.x - b.x) > Math.abs(a.y - b.y)) {
        val (p1, p2) = if (a.x < b.x) (a, b) else (b, a)

        p1.x.to(p2.x).foreach { a =>
          val y = p1.y + (a.toDouble - p1.x) / (p2.x - p1.x) * (p2.y - p1.y)

          val ya = y.ceil.toInt
          val yb = y.toInt

          grid(a)(ya) += l
          grid(a)(yb) += l
          if (a > 0) {
            grid(a - 1)(ya) += l
          }
          if (a < maxX) {
            grid(a + 1)(yb) += l
          }
        }
      } else {
        val (p1, p2) = if (a.y < b.y) (a, b) else (b, a)

        p1.y.to(p2.y).foreach { a =>
          val x = p1.x + (a.toDouble - p1.y) / (p2.y - p1.y) * (p2.x - p1.x)

          val xa = x.ceil.toInt
          val xb = x.toInt

          grid(xa)(a) += l
          grid(xb)(a) += l
          if (a > 0) {
            grid(xa)(a - 1) += l
          }
          if (a < maxY) {
            grid(xb)(a + 1) += l
          }
        }
      }
  }

  maxY.to(0).by(-1).foreach { y =>
    0.to(maxX).foreach { x =>
      print(grid(x)(y).size)
      print(" ")
    }
    println()
  }

  println("Computing collisions...")

  val collisionPoints = scala.collection.mutable.Set.empty[(Rational, Rational)]
  val seenPairs = scala.collection.mutable.Set.empty[Set[Line]]

  0.to(maxX).foreach { x =>
    println(x)
    maxY.to(0).by(-1).foreach { y =>
      grid(x)(y).toList.combinations(2).foreach {
        case List(l1@Line(Point(x1, y1), Point(x2, y2)), l2@Line(Point(x3, y3), Point(x4, y4))) =>

          if (!seenPairs(Set(l1, l2))) {
            seenPairs += Set(l1, l2)
            val d = (x1 - x2) * (y3 -  y4) - (y1 - y2) * (x3 - x4)

            if (d != 0) {
              val t = Rational((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4), d)
              val u = Rational((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3), d) * -1

              if (u > zero && u < one && t > zero && t < one) {
                collisionPoints += ((t * (x2 - x1) + x1, t * (y2 - y1) + y1))
              }
            }
          }
      }
    }
  }

  collisionPoints
}

// def slowSolve(lines: Set[Line]): scala.collection.mutable.Set[(Rational, Rational)] = {
//   var n = 0
//   val collisionPoints = scala.collection.mutable.Set.empty[(Rational, Rational)]

//   lines.toList.combinations(2).foreach {
//     case List(Line(Point(x1, y1), Point(x2, y2)), Line(Point(x3, y3), Point(x4, y4))) =>
//       val d = (x1 - x2) * (y3 -  y4) - (y1 - y2) * (x3 - x4)

//       if (d != 0) {
//         val t = Rational((x1 - x3) * (y3 - y4) - (y1 - y3) * (x3 - x4), d)
//         val u = Rational((x1 - x2) * (y1 - y3) - (y1 - y2) * (x1 - x3), d) * -1

//         if (u > zero && u < one && t > zero && t < one) {
//           collisionPoints += ((t * (x2 - x1) + x1, t * (y2 - y1) + y1))
//         }
//       }
//   }

//   collisionPoints
// }

// val i = allLinesSet.take(9)
// val b = solve(i)
// val a = slowSolve(i)

// println(a.map { case (x, y) => (x.toDouble, y.toDouble) })
// println(b.map { case (x, y) => (x.toDouble, y.toDouble) })

// println(a -- b)
// println(b -- a)

println(solve(allLinesSet).size)
