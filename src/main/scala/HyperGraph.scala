class HyperGraph(private[this] val nodes: Array[HyperVertex], private[this] val edges: Array[HyperEdge]) {

  this.calcSupp
  this.calcDSupp

  def getNodes : Array[HyperVertex] = nodes

  def getEdges : Array[HyperEdge] = edges

  def getSupp(a: Array[HyperVertex]): Int = {
    edges.map(k => k.getNodes.intersect(a)).filter(_.nonEmpty).size
  }

  def calcSupp = {
    for (n <- nodes) {
      n.supp = getSupp(Array(n))
    }

  }

  def calcDSupp() = {
    val nn = nodes.sortWith(_.supp < _.supp)
    var n1 = nn.take(0)
    for (n <- nodes.sortWith(_.supp < _.supp)) {
      n1 ++= n1.union(Array(n))
      n.dSupp = getSupp(n1)
    }
  }

  def getCardinality: Int = {
    edges.length
  }

  def getMaxClique: List[HyperVertex] = {
    nodes.toList.sortWith(_.supp < _.supp).filter( _.dSupp < getCardinality)
  }

  def getToExplore: List[HyperVertex] = {
    getNodes.toList.sortWith(_.supp < _.supp).diff(getMaxClique)
  }
}
