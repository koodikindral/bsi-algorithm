class Traversal(private[this] val edges: Array[HyperVertex]) {
  var supp: Int = 0
  var dSupp: Int = 0
  var sum: Int = edges.map(_.getValue).mkString("").toInt

  def getEdges: Array[HyperVertex] = edges

  override def toString: String =  {
    var s = "("
    edges.foreach(f => {
      s += f.toString
    })
    s += ", " + supp + ", " + dSupp + ", " + sum +")"
    s
  }
}
