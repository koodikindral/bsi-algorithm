import scala.collection.mutable.ArrayBuffer

class HyperEdge(_vertices: Iterable[Int]) {


  val vertices = _vertices.to[ArrayBuffer]
  val num = vertices.sortBy(f => f).mkString("").toInt
  var supp = 0
  var dSupp = 0

  def add(n: Int) : HyperEdge = {
    if (!vertices.contains(n)) {
      vertices += n
    }
    this
  }

  def remove(n: Int): HyperEdge = {
    if (vertices contains n) {
      vertices -= n
    }
    this
  }

  override def toString: String =  {
    vertices.mkString("", "", " [" + supp + ", " + dSupp + ", " + num + "]")
  }
}
