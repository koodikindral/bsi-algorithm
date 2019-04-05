import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer

object HyperGraphTest {

  def main(args : Array[String]): Unit = {
    val conf = new SparkConf().setAppName("Hypergraph greedy").setMaster("local")
    val sc = new SparkContext(conf)
    val edgesRDD = sc.textFile(getClass.getResource("/testdata1.txt").getPath).map(_.split(" ").map(_.toInt).toSet)

    getEdges(edgesRDD).foreach(e => println(e.toString))
  }

  def getEdges(vertices: RDD[Set[Int]]): Array[HyperEdge] = {
    vertices.collect.map(f => {
      new HyperEdge(f.map(new HyperVertex(_)))
    })
  }
}
