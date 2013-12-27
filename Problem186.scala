import scala.collection.mutable

object Problem186 {
  val N_USERS = 1000000
  val LIMIT = 5000000
  val PM = 524287

  val S = Array.fill(LIMIT)(0)

  val visited = mutable.Set[Int]()
  val toVisit = mutable.Queue[Int](PM)

  def numUsersReached(graph: mutable.Map[Int, mutable.Set[Int]]): Int = {
    while (toVisit.size > 0) {
      val n = toVisit.dequeue
      visited += n
      toVisit ++= graph.getOrElse(n, List[Int]()).filter(x => !visited.contains(x))
    }

    visited.size
  } 

  def main(args: Array[String]) {
    println("computing...")
    (0 to 55).foreach { i =>
      val k = ((100003 - 200003L * i + 300007L * i * i * i) % 1000000).toInt
      S(i) = k
    }
    (56 until LIMIT).foreach(k => S(k) = (S(k - 24) + S(k - 55)) % 1000000)
    println("done")

    val graph = mutable.Map[Int, mutable.Set[Int]]()

    var recNr = 1
    var numSuccessful = 0
    while (true) {
      val (caller, called) = (S(2 * recNr - 1), S(2 * recNr))
      if (caller != called) {
        numSuccessful += 1

        graph.getOrElseUpdate(caller, mutable.Set[Int]()) += called
        graph.getOrElseUpdate(called, mutable.Set[Int]()) += caller

        if (visited.contains(caller))
          toVisit += caller
        if (visited.contains(called))
          toVisit += called

        val reached = numUsersReached(graph)
        if (reached >= 99 * N_USERS / 100) {
          println(numSuccessful)
          return
        }
      }

      recNr += 1
    }
  }
}
