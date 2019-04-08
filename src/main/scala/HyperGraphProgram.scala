import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object HyperGraphProgram {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("Hypergraph greedy").setMaster("local")
    val sc = new SparkContext(conf)
    val edgesRDD = sc.textFile(getClass.getResource("/testdata1.txt").getPath).map(_.split(" ").map(_.toInt).toSet)

    val vertices: Array[HyperVertex] = getVertices(edgesRDD)
    val edges: Array[HyperEdge] = getEdges(edgesRDD)
    val hyperGraph: HyperGraph = createHyperGraph(vertices, edges)

    println("ITERATION #1")

    printVertices(hyperGraph)
    printEdges(hyperGraph)

    print("Max clique: ")
    println(hyperGraph.getMaxClique.map(_.getValue))

    print("To-explore: ")
    println(hyperGraph.getToExplore.map(_.getValue))
  }

  def printVertices(graph: HyperGraph) = {
    println("HyperVertices:")
    graph.getNodes.map(f => {
      println(f)
    })
  }

  def printEdges(graph: HyperGraph) = {
    println("HyperEdges:")
    graph.getEdges.map(f => {
      println(f.getNodes)
    })
  }

  def getEdges(edges: RDD[Set[Int]]): Array[HyperEdge] = {
    edges.collect.map(f => {
      new HyperEdge(f.map(new HyperVertex(_)))
    })
  }

  def getVertices(edges: RDD[Set[Int]]): Array[HyperVertex] = {
    edges.flatMap(f => f).distinct.sortBy(k => k, ascending = true).collect.map(f => {
      new HyperVertex(f)
    })
  }

  /*
  TODO: Create hypergraph from input file
   */
  def createHyperGraph(vertices: Array[HyperVertex], edges: Array[HyperEdge]): HyperGraph = {
    val n1 = new HyperVertex(1)
    val n2 = new HyperVertex(2)
    val n3 = new HyperVertex(3)
    val n4 = new HyperVertex(4)
    val n5 = new HyperVertex(5)
    val n6 = new HyperVertex(6)
    val n7 = new HyperVertex(7)
    val n8 = new HyperVertex(8)

    val e1 = new HyperEdge(Array(n1, n2))
    val e2 = new HyperEdge(Array(n2, n3, n7))
    val e3 = new HyperEdge(Array(n3, n4, n5))
    val e4 = new HyperEdge(Array(n4, n6))
    val e5 = new HyperEdge(Array(n6, n7, n8))
    val e6 = new HyperEdge(Array(n7))

    //new HyperGraph(vertices, edges)
    new HyperGraph(Array(n1, n2, n3, n4, n5, n6, n7, n8), Array(e1, e2, e3, e4, e5, e6))
  }


}
