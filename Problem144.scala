import scala.collection.mutable.Map
import scala.collection.mutable.Set

case class Vec(x: Double, y: Double) {
  def length = Math.sqrt(lengthSq)
  def lengthSq = Math.pow(x, 2) + Math.pow(y, 2)

  def normed = {
    val l = length
    Vec(x / l, y / l)
  }

  def dot(v: Vec) = {
    x * v.x + y * v.y
  }

  def `+`(v: Vec) = {
    Vec(x + v.x, y + v.y)
  }

  def `-`(v: Vec) = {
    Vec(x - v.x, y - v.y)
  }

  def `*`(a: Double) = {
    Vec(x * a,  y * a)
  }
}

val bounceIterator = new Iterator[Vec] {
  var start = Vec(0, 10.1)
  var end = Vec(1.4, -9.6)
  // start = Vec(1.4, -9.6)
  // end = Vec(-4, -6)

  def next = {
    val nb = nextBounce()
    start = end
    end = nb
    start
  }

  def nextBounce(): Vec = {
    println()
    // Facing inwards
    println("end", end)
    val ellipseDir =  Vec(4 * end.x, end.y) * -1
    println("normal dir", ellipseDir)
    val rayDir = (end - start)
    println("came from", start)
    println("ray dir", rayDir)

    val reflectionDir = (rayDir - ellipseDir * (2 * rayDir.dot(ellipseDir) / ellipseDir.lengthSq))

    println("reflection", reflectionDir)

    val a = 5
    val b = 10

    val m = reflectionDir.y / reflectionDir.x
    val m2 = reflectionDir.y * reflectionDir.y / (reflectionDir.x * reflectionDir.x)
    val c = end.y - reflectionDir.y * end.x / reflectionDir.x

    println(m, c)

    val det = a * b * Math.sqrt(a * a * m2 + b * b - c * c)
    val denom = b * b + a * a * m2

    val x1 = (-a * a * m * c - det) / denom
    val x2 = (-a * a * m * c + det) / denom

    println(det, denom)

    val y1 = m * x1 + c
    val y2 = m * x2 + c

    val v1 = Vec(x1, y1)
    val v2 = Vec(x2, y2)

    if ((v1 - end).length < (v2 - end).length)
      return v2
    else
      return v1
  }

  def hasNext = {
    val n = nextBounce()
    !(n.x >= -0.01 && n.x <= 0.01)
  }
}

val l = bounceIterator.toList
println(l)
println(l.length)
