import scala.collection.mutable.ArrayBuffer

class HyperGraph(private[this] val _vertices: Array[BigInt], private[this] val _edges: Array[HyperEdge]) {

  val vertices = _vertices
  val edges = _edges
  val cardinality = edges.length

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

  def calcEssentiality(_edges: ArrayBuffer[BigInt], _length: Int): Int = {
    val edges = _edges.toSet.subsets(_length)
    val max = edges.map(f => getSupp(f.toArray)).reduceLeft(_ max _)
    max
  }

  def traverse(maxClique: Array[HyperEdge], toExplore: Array[HyperEdge], mt: ArrayBuffer[HyperEdge]): ArrayBuffer[HyperEdge] = {


    var eList = toExplore.to[ArrayBuffer]
    toExplore.par.foreach(t => {

      //println("Exploring: " + t)
      // remove currently explorable element from max-clique
      eList -= t

      // create pairs with max-clique & the rest of to-explore list

      val pairs = calcDJSupp(
        (maxClique ++ eList).map(f => new HyperEdge(t.vertices.union(f.vertices).distinct, calcEssentiality(t.vertices.union(f.vertices).distinct, t.vertices.size)))
      ).filter(f => f.supp > f.ess)

      // remove elements that have 1. ess. condition fulfilled
      // check for 2.nd ess. condition (cardinality)
      val explore = pairs.filter(f => f.dSupp.equals(cardinality))
      val mtList = explore.filter(_.supp.equals(cardinality))

      val newExplore = explore.diff(mtList)
      val clique = pairs.diff(explore ++ mtList)
      mt ++= mtList

      if (!clique.isEmpty && !newExplore.isEmpty) {
        mt ++= traverse(clique, newExplore, mt)
      }
    })

    mt.distinct
  }
}
