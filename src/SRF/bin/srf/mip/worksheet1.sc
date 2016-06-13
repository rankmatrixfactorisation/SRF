package cs.kuleuven.mip

import cs.kuleuven.matrix._
import cs.kuleuven.utils._
import util.Random.shuffle

import cs.kuleuven.clustering._

object worksheet1 {
 
 /*val l1 = (0 until 20).map(t => 0).toList
 val l2 = (10 until 15).map(t => t).toList
 
 val l3 = shuffle(l1 ++ l2)
 val t = java.lang.Math.round(5/2)
 
 val kmeans = new KMeans("/home/thanh/repos/git/pintmatrixdecomp/data/syn/toy/matthijs.txt", 2)
 //kmeans.updateCentroids()
 kmeans.doClustering(3)
 
 val profiles = kmeans.getClusterProfileMatrix
 profiles.printMatrix
 
 val assignments = kmeans.getClusterAsignmentMatrix
 assignments.printMatrix
 
 */
 val a = 0 until 3                                //> a  : scala.collection.immutable.Range = Range(0, 1, 2)
  val t = Vector(3,2,5,1)                         //> t  : scala.collection.immutable.Vector[Int] = Vector(3, 2, 5, 1)
 val p = t.product                                //> p  : Int = 30
 
 val s = ""                                       //> s  : String = ""
 val b = s.isEmpty()                              //> b  : Boolean = true
 
}