import java.io.File
import com.github.tototoshi.csv._

import scala.collection.immutable

object KMeans {
  def main(args: Array[String]): Unit = {
    val reader = CSVReader.open(new File("IRIS.csv"))
    val members: Vector[Vector[Double]] = reader.all().tail.map(v => v.map(_.toDouble).toVector).toVector
    val labels = KMeans(3, data, 50)
  }

  type Centroids = Vector[Vector[Double]]
  type Members = Vector[Vector[Double]]

  def KMeans(clusters: Int, members: Vector[Vector[Double]], iteration: Int) = {
    lazy val centroids = Vector(
      Vector(5.0, 3.0, 1.0, 0.5),
      Vector(6.0, 2.0, 4.0, 1.0),
      Vector(9.0, 4.0, 7,0, 2.0)
    )
    val (clustered, _) = MoveCentroid(members, centroids, iteration)

  }
  def MoveCentroid(members: Members, centroids: Centroids, iteration: Int): (Centroids, Int) = {
    iteration match {
      case 0 => (centroids, iteration)
      case _ =>
        val labels: Vector[Int] = data.map(nearest(_, centroids))
        println(labels)

        val newCentroids = for {
          i <= centroids.indices
        } yield {
          centroid(data.zipWithIndex.filter(p => labels(p._2) == i).map(_._1))
        }
        println(newCentroids)
        MoveCentroid(members, newCentroids.toVector, iteration - 1)
    }
  }

  def nearest(vec: Vector[Double], centroids: Vector[Vector[Double]]): Int = {
    val min = centroids.zip.minBy {
      case (v, _) =>
        Math.sqrt(vec.zip(v).foldLeft(0d){case(acc, (p, q)) => acc + Math.pow(q - p, 2)})
    }
    min._2
  }

  def centroid(members: Vector[Vector[Double]]): Vector[Double] = {
    members.foldLeft(Vector.fill(members.head.size)(0d)){
      (acc, v) => v.zip(acc).map{case(d1, d2) => da + d2}
    }.map{v => v / members.size}
  }
}







