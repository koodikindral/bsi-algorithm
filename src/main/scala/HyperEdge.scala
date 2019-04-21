import scala.collection.mutable.ArrayBuffer

class HyperEdge(_vertices: Iterable[BigInt], _ess: Int) {


  val vertices = _vertices.to[ArrayBuffer]
  val num = BigInt(vertices.sortBy(f => f).mkString(""))
  var supp = 0
  var dSupp = 0
  var ess = _ess

  def add(n: BigInt) : HyperEdge = {
    if (!vertices.contains(n)) {
      vertices += n
    }
    this
  }

  def add(n: Array[BigInt]) : HyperEdge = {
    n.map(add)
    this
  }

  def remove(n: BigInt): HyperEdge = {
    if (vertices contains n) {
      vertices -= n
    }
    this
  }

  override def toString: String =  {
    vertices.mkString("", "", " [" + supp + ", " + dSupp + ", " + num + "] (" + ess +")")
  }
}
