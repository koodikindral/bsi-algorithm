import scala.collection.mutable.ArrayBuffer

class HyperGraph(private[this] val _vertices: Array[Int], private[this] val _edges: Array[HyperEdge]) {

  val vertices = _vertices
  val edges = _edges
  val cardinality = edges.size
  var visited = ArrayBuffer[Int]()

  def getSupp(_edge: Array[Int]): Int = edges.map(e => e.vertices.intersect(_edge)).count(_.nonEmpty)

  def calcDJSupp(_edges: Array[HyperEdge]): Array[HyperEdge] = {

    _edges.foreach(f => {
      f.supp = getSupp(f.vertices.toArray)
    })

    var count = Array[Int]()
    _edges.sortBy(f => f.supp).foreach(f => {
      count ++= count.union(f.vertices)
      f.dSupp = getSupp(count)
    })
    _edges
  }


  def traverse(maxClique: Array[HyperEdge], toExplore: Array[HyperEdge], mt: ArrayBuffer[HyperEdge]): ArrayBuffer[HyperEdge] = {


    var eList = toExplore.to[ArrayBuffer]
    toExplore.foreach(t => {

      eList -= t
      println("Exploring: " + t.toString)
      (maxClique ++ eList).foreach(f => println(f))

      if (maxClique.isEmpty) {
          mt += t
      } else {
        // create pairs with max-clique and to explore list, filter out ones that have the same dsupp (Ess. condition 1)
        val pairs = calcDJSupp(
          (maxClique ++ eList).map(f => new HyperEdge(t.vertices.union(f.vertices).distinct))
        ).filterNot(_.supp.equals(t.supp))

        // check for 2.nd ess. condition (cardinality)
        val explore = pairs.filter(_.dSupp.equals(cardinality))
        val clique = pairs.diff(explore).distinct

        val tr = traverse(clique, explore, mt)

      }
    })
    mt
  }
}
