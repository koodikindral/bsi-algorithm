import scala.collection.mutable.ArrayBuffer

class HyperEdge(_nodes: Iterable[HyperVertex]) {
  private val nodes = new ArrayBuffer[HyperVertex]
  _nodes.foreach(v => add(v))

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
}
