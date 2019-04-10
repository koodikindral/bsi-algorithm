import scala.collection.mutable.ArrayBuffer

class HyperGraph(private[this] val nodes: Array[HyperVertex], private[this] val edges: Array[HyperEdge]) {

  var traversals = nodes.map(f => {
    new Traversal(Array(f))
  })

  var mtList = ArrayBuffer[Traversal]()

  def getNodes : Array[HyperVertex] = nodes

  def getEdges : Array[HyperEdge] = edges

  def getSupp(a: Array[HyperVertex]): Int = edges.map(k => k.getNodes.intersect(a)).count(_.nonEmpty)

  def calcDJSupp(t: Array[Traversal]): Array[Traversal] = {

    t.foreach(f => {
      f.supp = getSupp(f.getEdges)
    })

    var temp = Array[HyperVertex]()

    t.sortBy(f => (f.supp)).foreach(f => {
      temp ++= temp.union(f.getEdges)
      f.dSupp = getSupp(temp)
    })

    t
  }

  def getCardinality: Int = {
    edges.length
  }

  def traverse(maxClique: Array[Traversal], toExplore: Array[Traversal], mt: ArrayBuffer[Traversal]): ArrayBuffer[Traversal] = {
    toExplore.foreach(t => {
      if (t.dSupp.equals(getCardinality) && maxClique.isEmpty) {
        mt ++= ArrayBuffer(t)
      } else {

        println("Traversing: " + t.toString)

        val temp = calcDJSupp(
          maxClique.map(f => new Traversal(t.getEdges.union(f.getEdges).distinct))
        ).filter(!_.supp.equals(t.supp))

        val explore = temp.filter(_.dSupp.equals(getCardinality))
        val clique = temp.filter(!_.dSupp.equals(getCardinality))

        traverse(clique, explore, mt)
      }
    })
    mt
  }
}
