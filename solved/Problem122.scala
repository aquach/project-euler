import scala.collection.concurrent.TrieMap
import scala.collection.immutable.Set

object Problem122 {
  def main(x: Array[String]) {
    val paths = TrieMap[Int, Set[Set[Int]]]()

    paths(1) = Set(Set(1))

    val MAX = 200
    for (k <- 2 to MAX) {
       val possiblePaths = (1 to k / 2).toSet.flatMap((m: Int) => {
        val n = k - m
        for (mPath: Set[Int] <- paths(m); nPath: Set[Int] <- paths(n))
          yield mPath ++ nPath + k
      })

       val sizeOfBestPath = possiblePaths.toIterator.map(_.size).min

       val bestPaths: Set[Set[Int]] = possiblePaths.filter(_.size == sizeOfBestPath)

       paths(k) = bestPaths

       println((k, bestPaths.size))
    }

    println((2 to MAX).map(paths).map(_.head.size - 1).sum)
  }
}