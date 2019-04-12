import scala.collection.mutable.ArrayBuffer

class HyperGraph(private[this] val _vertices: Array[Int], private[this] val _edges: Array[HyperEdge]) {

  val vertices = _vertices
  val edges = _edges
  val cardinality = edges.length
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

    toExplore.foreach(t => {
      println("Exploring: " + t.toString)

      if (maxClique.isEmpty && !visited.contains(t.num)) {
          mt ++= Array(t)
      } else {

        val pairs = calcDJSupp(
          (toExplore.drop(1).filterNot(_.equals(t)) ++
            maxClique).distinct.map(f => new HyperEdge((t.vertices.union(f.vertices).distinct)))
        ).filterNot(_.supp.equals(t.supp))

        val explore = pairs.filter(_.dSupp.equals(cardinality))
        val clique = pairs.diff(explore).distinct

        traverse(clique, explore, mt)
      }
      visited += t.num
    })
    mt
  }
}
