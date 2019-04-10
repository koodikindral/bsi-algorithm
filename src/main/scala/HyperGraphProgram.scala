import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer

object HyperGraphProgram {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("Hypergraph greedy").setMaster("local")
    val sc = new SparkContext(conf)
    val edgesRDD = sc.textFile(getClass.getResource("/testdata1.txt").getPath)
      .map(_.split(" ").map(_.toInt).toSet)

    val vertices: Array[HyperVertex] = getVertices(edgesRDD)
    val edges: Array[HyperEdge] = getEdges(edgesRDD, vertices)

    val hyperGraph: HyperGraph = createHyperGraph(vertices, edges)

    val maxClique = hyperGraph.calcDJSupp(vertices.map(f => new Traversal(Array(f))))
    val toExplore = maxClique.filter(_.dSupp >= hyperGraph.getCardinality)

    val mt = hyperGraph.traverse(maxClique.filter(_.dSupp < hyperGraph.getCardinality), toExplore, ArrayBuffer[Traversal]())

    println("Minimal traversals: " + mt.size)
    println("Result: ")
    mt.foreach(f => print(f.sum + ", "))
    println()
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
