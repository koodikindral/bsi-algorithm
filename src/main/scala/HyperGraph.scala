import scala.collection.mutable.ArrayBuffer

class HyperGraph(private[this] val _vertices: Array[BigInt], private[this] val _edges: Array[HyperEdge]) {

  val vertices = _vertices
  val edges = _edges
  val cardinality = edges.size

  def getSupp(_edge: Array[BigInt]): Int = edges.map(e => e.vertices.intersect(_edge)).count(_.nonEmpty)

  def calcDJSupp(_edges: Array[HyperEdge]): Array[HyperEdge] = {

    _edges.foreach(f => {
      f.supp = getSupp(f.vertices.toArray)
    })

    var count = Array[BigInt]()
    _edges.sortBy(f => f.supp).foreach(f => {
      count ++= count.union(f.vertices)
      f.dSupp = getSupp(count)
    })
    _edges
  }


  def traverse(maxClique: Array[HyperEdge], toExplore: Array[HyperEdge], mt: ArrayBuffer[HyperEdge]): ArrayBuffer[HyperEdge] = {

    var eList = toExplore.to[ArrayBuffer]
    toExplore.foreach(t => {

      // remove currently explorable element from max-clique
      eList -= t

      if (t.supp == cardinality) {
          mt += t
      } else {

        // create pairs with max-clique & the rest of to-explore list
        val pairs = calcDJSupp(
          (maxClique ++ eList).map(f => new HyperEdge(t.vertices.union(f.vertices).distinct))
        ).filterNot(_.supp.equals(t.supp)) // remove elements that have 1. ess. condition fulfilled

        // check for 2.nd ess. condition (cardinality)
        val explore = pairs.filter(_.dSupp.equals(cardinality))
        val clique = pairs.filterNot(_.dSupp.equals(cardinality))

        mt ++= traverse(clique, explore, mt)
      }
    })
    mt.distinct
  }
}
