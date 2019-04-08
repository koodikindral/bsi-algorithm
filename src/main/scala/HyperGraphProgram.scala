import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

object HyperGraphProgram {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("Hypergraph greedy").setMaster("local[4]")
    val sc = new SparkContext(conf)
    val edgesRDD = sc.textFile(getClass.getResource("/testdata1.txt").getPath)
      .map(_.split(" ")
        .map(_.toInt).toSet)

    val vertices: Array[HyperVertex] = getVertices(edgesRDD)
    val edges: Array[HyperEdge] = getEdges(edgesRDD, vertices)


    val hyperGraph: HyperGraph = createHyperGraph(vertices, edges)

    println("ITERATION #1")

    printTraversals(hyperGraph)
    printVertices(hyperGraph)
    printEdges(hyperGraph)

    val maxClique = hyperGraph.getMaxClique
    val toExplore = hyperGraph.getToExplore

    println("Max clique: ")
    maxClique.foreach(f => println(f.toString))

    println("To-explore: ")
    toExplore.foreach(f => println(f.toString))

    hyperGraph.traversals = maxClique.map(f => new Traversal(toExplore.take(1)(0).getEdges.union(f.getEdges)))

    println("ITERATION #2")
    hyperGraph.calcSupp()
    hyperGraph.calcDSupp()
    println("Max clique: ")
    hyperGraph.getMaxClique.foreach(f => println(f.toString))

    println("To-explore: ")
    hyperGraph.getToExplore.foreach(f => println(f.toString))
  }

  def printVertices(graph: HyperGraph): Unit = {
    println("HyperVertices:")
    graph.getNodes.foreach(f => {
      println(f)
    })
  }

  def printTraversals(graph: HyperGraph): Unit = {
    println("Traversals:")
    graph.traversals.foreach(f => println(f.toString))
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
