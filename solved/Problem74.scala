import scala.collection.mutable.Map
import scala.collection.mutable.Set

object Problem74 {

  case class Memo[A, B](f: A => B) extends (A => B) {
    private val cache = scala.collection.mutable.Map.empty[A, B]
    def apply(x: A) = cache.getOrElseUpdate(x, f(x))
  }

  def main(args: Array[String]) {
    lazy val fact: Memo[Int, Int] = Memo {
      case 0 => 1
      case n => n * fact(n - 1)
    }

    println((0 until 1000000).count({ i =>
      val seen = scala.collection.mutable.Set.empty[Int]
      var x = i
      while (!seen.contains(x)) {
        seen += x
        x = x.toString.map(x => fact(x - '0')).sum
      }

      seen.size == 60
    }))
  }
}
