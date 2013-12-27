object Problem186 {

  class DisjointSets(numNodes: Int) {
    val parents = Array.fill(numNodes)(0)
    val setSizes = Array.fill(numNodes)(0)
    (0 until numNodes).foreach { i => parents(i) = i; setSizes(i) = 1 }

    def findRoot(node: Int): Int = {
      if (parents(node) == node)
        node
      else {
        val root = findRoot(parents(node))
        parents(node) = root
        root
      }
    }

    def numNodesInSet(node: Int): Int =
      setSizes(findRoot(node))

    def union(node1: Int, node2: Int) {
      val node1Root = findRoot(node1)
      val node2Root = findRoot(node2)

      if (node1Root != node2Root) {
        parents(node2Root) = node1Root
        setSizes(node1Root) += setSizes(node2Root)
      }
    }
  }


  def main(args: Array[String]) {
    val N_USERS = 1000000
    val LIMIT = 5000000
    val PM = 524287

    val S = Array.fill(LIMIT)(0)
    (0 to 55).foreach { i =>
      val k = ((100003 - 200003L * i + 300007L * i * i * i) % 1000000).toInt
      S(i) = k
    }
    (56 until LIMIT).foreach(k => S(k) = (S(k - 24) + S(k - 55)) % 1000000)

    val sets = new DisjointSets(N_USERS)

    var recNr = 1
    var numSuccessful = 0
    while (true) {
      val (caller, called) = (S(2 * recNr - 1), S(2 * recNr))
      if (caller != called) {
        numSuccessful += 1

        sets.union(caller, called)

        val reached = sets.numNodesInSet(PM)
        if (reached >= 99 * N_USERS / 100) {
          println(numSuccessful)
          return
        }
      }

      recNr += 1
    }
  }
}
