import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer

object HyperGraphProgram {

  def main(args: Array[String]): Unit = {
    val conf = new SparkConf().setAppName("Hypergraph greedy").setMaster("local[1]")
    val sc = new SparkContext(conf)


    val edgesRDD = sc.textFile(getClass.getResource("/testdata1.txt").getPath)
      .map(_.split(" ").map(f => BigInt(f)).toSet)


    val edges: Array[HyperEdge] = getEdges(edgesRDD)

    val hyperGraph: HyperGraph = createHyperGraph(edges)


    println("Cardinality: " + hyperGraph.cardinality)
    val maxClique = hyperGraph.calcDJSupp(hyperGraph.vertices.map(f => new HyperEdge(Array(f))))
    val toExplore = maxClique.filter(_.dSupp.equals(hyperGraph.cardinality))

    val mt = hyperGraph.traverse(maxClique.filter(f => f.dSupp < hyperGraph.cardinality).sortBy(f => f.supp), toExplore, ArrayBuffer[HyperEdge]())

    println("-----")
    println("Minimal traversals: " + mt.distinct.size)
    println("Result: ")
    mt.distinct.foreach(f => println(f.toString))
    println()
  }

  def getEdges(edges: RDD[Set[BigInt]]): Array[HyperEdge] = {
    edges.collect.map(f => {
      new HyperEdge(f)
    })
  }

  def createHyperGraph(edges: Array[HyperEdge]): HyperGraph = {
    val vertices = edges.flatMap(f => f.vertices).distinct
    new HyperGraph(vertices, edges)
  }
}
