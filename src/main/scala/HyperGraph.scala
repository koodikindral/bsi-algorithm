class HyperGraph(private[this] val nodes: Array[HyperVertex], private[this] val edges: Array[HyperEdge]) {

  var traversals = nodes.map(f => {
    new Traversal(Array(f))
  })

  this.calcSupp()
  this.calcDSupp()

  def getNodes : Array[HyperVertex] = nodes

  def getEdges : Array[HyperEdge] = edges

  def getSupp(a: Array[HyperVertex]): Int = edges.map(k => k.getNodes.intersect(a)).count(_.nonEmpty)


  def calcSupp(): Unit = {
    traversals.foreach(f => {
      f.supp = getSupp(f.getEdges)
    })
  }

  def calcDSupp(): Unit = {
    var temp = Array[HyperVertex]()
    traversals.sortWith(_.supp < _.supp).foreach(f => {
      temp ++= temp.union(f.getEdges)
      f.dSupp = getSupp(temp)
    })
  }

  def getCardinality: Int = {
    edges.length
  }

  def getMaxClique: Array[Traversal] = {
    traversals.filter(_.dSupp < getCardinality).sortWith(_.supp < _.supp)
  }

  def getToExplore: Array[Traversal] = {
    traversals.sortWith(_.supp < _.supp).diff(getMaxClique)
  }
}
