import scala.collection.mutable.ArrayBuffer

class HyperEdge(_nodes: Iterable[HyperVertex]) {
  private val nodes = _nodes.to[ArrayBuffer]

  def getNodes: ArrayBuffer[HyperVertex] = nodes

  def add(v: HyperVertex) : HyperEdge = {
    if (!nodes.contains(v)) {
      nodes += v
    }
    this
  }

  def remove(v: HyperVertex): HyperEdge = {
    if (nodes contains v) {
      nodes -= v
    }
    this
  }

  override def toString: String =  {
    nodes.map(n => n.getValue).mkString("(", ", ", ")")
  }
}
