import java.io.File
import com.github.tototoshi.csv._

object KMeans {
  def main(args: Array[String]): Unit = {
    val reader = CSVReader.open(new File("IRIS.csv"))
    val data: Vector[Vector[Double]] = reader.all().tail.map(v => v.map(_.toDouble).toVector).toVector

    val labels = KMeans(3, data, 10)
    println(labels)

  }

  type Centroids = Vector[Vector[Double]]
  type Data      = Vector[Vector[Double]]
  type Labels    = Vector[Int]

  def KMeans(clusters: Int, data: Data, iteration: Int): Labels = {
    lazy val centroids = Vector(
      Vector(5.0, 3.5, 1.0, 0.1),
      Vector(5.5, 2.0, 3.8, 1.1),
      Vector(8.2, 4.2, 7.1, 3.0)
    )
    val (clustered: Centroids, _: Int) = MoveCentroid(data, centroids, iteration)
    data.map(nearest(clustered, _))

  }

  def MoveCentroid(data: Data, centroids: Centroids, iteration: Int): (Centroids, Int) = {
    iteration match {
      case 0 => (centroids, iteration)
      case _ =>
        val labels: Vector[Int] = data.map(nearest(centroids, _))

        val newCentroids = for {
          i <- centroids.indices
        } yield {
          centroid(data.zipWithIndex.filter(p => labels(p._2) == i).map(_._1))
        }
        println(newCentroids)
        println("----------------------------------------------------------------------")
        MoveCentroid(data, newCentroids.toVector, iteration - 1)
    }
  }

  def nearest(centroids: Centroids, vec: Vector[Double]): Int = {
    val m = centroids.zipWithIndex.minBy {
      case (v, _) =>
        Math.sqrt(vec.zip(v).foldLeft(0d){ case (acc, (p, q)) => acc + Math.pow(q - p, 2) })
    }
    m._2
  }

  def centroid(data: Data): Vector[Double] = {
    data.foldLeft(Vector.fill(data.head.size)(0d)){ (acc, v) =>
      v.zip(acc).map{ case (d1, d2) => d1 + d2 }
    }.map { v =>
      v / data.size
    }
  }
}
