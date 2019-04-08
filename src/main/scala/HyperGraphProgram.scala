import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object HyperGraphProgram {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("Hypergraph greedy").setMaster("local[1]")
    val sc = new SparkContext(conf)
    val edgesRDD = sc.textFile(getClass.getResource("/testdata1.txt").getPath)
      .map(_.split(" ")
        .map(_.toInt).toSet)

    val vertices: Array[HyperVertex] = getVertices(edgesRDD)
    val edges: Array[HyperEdge] = getEdges(edgesRDD, vertices)


    val hyperGraph: HyperGraph = createHyperGraph(vertices, edges)

    println("ITERATION #1")

    printVertices(hyperGraph)
    printEdges(hyperGraph)

    print("Max clique: ")
    println(hyperGraph.getMaxClique.map(_.getValue))

    print("To-explore: ")
    println(hyperGraph.getToExplore.map(_.getValue))
  }

  def printVertices(graph: HyperGraph): Unit = {
    println("HyperVertices:")
    graph.getNodes.foreach(f => {
      println(f)
    })
  }

  def printEdges(graph: HyperGraph): Unit = {
    println("HyperEdges:")
    graph.getEdges.foreach(f => {
      println(f.getNodes)
    })
  }

  def getEdges(edges: RDD[Set[Int]], vertices: Array[HyperVertex]): Array[HyperEdge] = {
    val vertexMap = vertices.groupBy(_.getValue)

    edges.collect.map(f => {
      new HyperEdge(f.flatMap(vertexMap(_)))
    })
  }

  def getVertices(edges: RDD[Set[Int]]): Array[HyperVertex] = {
    edges.flatMap(f => f).distinct.sortBy(k => k, ascending = true).collect.map(f => {
      new HyperVertex(f)
    })
  }

  def createHyperGraph(vertices: Array[HyperVertex], edges: Array[HyperEdge]): HyperGraph = {
    new HyperGraph(vertices, edges)
  }
}
