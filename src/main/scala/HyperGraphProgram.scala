import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer

object HyperGraphProgram {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("Hypergraph greedy").setMaster("local[*]")
    val sc = new SparkContext(conf)


    val edgesRDD = sc.textFile(getClass.getResource("/testdata3.txt").getPath)
      .map(_.split(" ").map(f => BigInt(f)).toSet)

    try {
      val edges: Array[HyperEdge] = getEdges(edgesRDD)

      val hyperGraph: HyperGraph = createHyperGraph(edges)

      val maxClique = hyperGraph.calcDJSupp(hyperGraph.vertices.map(f => new HyperEdge(Array(f), 0)))
      val toExplore = maxClique.filter(_.dSupp.equals(hyperGraph.cardinality))

      val mt = hyperGraph.traverse(maxClique.filter(f => f.dSupp < hyperGraph.cardinality), toExplore, ArrayBuffer[HyperEdge]())

      println("Hypergraph: " + hyperGraph.edges.size + " edges, " + hyperGraph.vertices.size)
      println("Cardinality: " + hyperGraph.edges.size)
      println("Minimal traversals: " + mt.distinct.size)
      println("Result: ")
      mt.distinct.sortBy(f => f.num).foreach(f => println(f.toString))
      println()
    } finally {
      sc.stop()
    }

  }

  def getEdges(edges: RDD[Set[BigInt]]): Array[HyperEdge] = {
    edges.collect.map(f => {
      new HyperEdge(f, 0)
    })
  }

  def createHyperGraph(edges: Array[HyperEdge]): HyperGraph = {
    val vertices = edges.flatMap(f => f.vertices).distinct
    new HyperGraph(vertices, edges)
  }
}
