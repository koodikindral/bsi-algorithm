class HyperGraph(private[this] val nodes: Array[HyperVertex], private[this] val edges: Array[HyperEdge]) {

  this.calcSupp()
  this.calcDSupp()

  def getNodes : Array[HyperVertex] = nodes

  def getEdges : Array[HyperEdge] = edges

  def getSupp(a: Array[HyperVertex]): Int = edges.map(k => k.getNodes.intersect(a)).count(_.nonEmpty)

  def calcSupp(): Unit = {
    nodes.foreach(f => {
      f.supp = getSupp(Array(f))
    })
  }

  def calcDSupp(): Unit = {
    val nn = nodes.sortWith(_.supp < _.supp)
    var n1 = nn.take(0)

    nodes.sortWith(_.supp < _.supp).foreach(f => {
      n1 ++= n1.union(Array(f))
      f.dSupp = getSupp(n1)
    })
  }

  def getCardinality: Int = {
    edges.length
  }

  def getMaxClique: List[HyperVertex] = {
    nodes.toList.filter(_.dSupp < getCardinality).sortWith(_.supp < _.supp)
  }

  def getToExplore: List[HyperVertex] = {
    nodes.toList.sortWith(_.supp < _.supp).diff(getMaxClique)
  }
}
