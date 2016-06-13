package srf.clustering

import util.Random.shuffle
import scala.util.Random
import srf.matrix._

/**
 * Created on November 26, 2014
 */

class KMeans (filename: String, k: Int) {
  
  val m = new DenseMatrix(filename, "\t")
  val K = 0 until k
  val Rows = 0 until m.rowSize
  val Cols = 0 until m.colSize
  
  val rand = new Random()
  var centroids = new Array[List[Int]](k)
  var assignments = new Array[Int](m.rowSize)
  
  
  def getFootruleDist(v1: List[Int], v2: List[Int]): Int = {
    
    if (v1.length != v2.length) {
      throw new Exception("Two vectors do not have the same length")
    } 
    
    val dist = (0 until v1.length).map(i => java.lang.Math.abs(v1(i) - v2(i))).sum
    
    dist
  }
  
  def updateCentroids() {
    
    if (!isInitialized) {
      //printCentroids()
      // random initialization
      while (!isInitialized) {
        for (i <- 0 until m.rowSize) {
          assignments(i) = Random.nextInt(k)
        }
      }
      //assignments.foreach(x => println(x))
      //printClusterMember
    }
    
    // update centroids
    for (i <- K) {
      val clusterMem = Rows.filter(r => assignments(r) == i)
      if (clusterMem.length > 0) {
        val colSum = Cols.map(c => clusterMem.map(r => m.at(r, c)).sum).toList
        val centroidI = colSum.map(c => java.lang.Math.round(c/clusterMem.length))
        centroids(i) = centroidI
      } else {
        centroids(i) = List.fill(Cols.length)(0)
      }
    }
    
    //println("Current centroids")
    //printCentroids()
    
  }
  
  def updateAssignments() {
    
    val rowDistances = Rows.map(r => K.map(cluster => this.getFootruleDist(m.getRowVector(r), centroids(cluster))).toList).toList
    val sortedRowDistances = rowDistances.map(rowDists => rowDists.sorted)
    val newAssignments = Rows.map(r => rowDistances(r).indexOf(sortedRowDistances(r)(0)))
    
    Rows.map(r => assignments(r) = newAssignments(r))
    
  }
  
  def collectClusterMember(): List[List[Int]] = {
    val cMembers = K.map(i => Rows.filter(r => assignments(r) == i).toList).toList
    cMembers
  }
  
  def doClustering(repeats: Int) = {
    
    for (i <- 0 until repeats) {
      updateCentroids()
      updateAssignments()
    }
    
    println("KMeans clustering results:")
    println("Member assingments: ")
    printClusterMember()
    println("Centroids: ")
    printCentroids
  }
  
  def save(filename: String) {
    
  }
  
  def isInitialized() = if (assignments.sum > 0) true else false
  
  def printClusterMember() {
    val clustMem = collectClusterMember
    clustMem.foreach(x => println(x))
  }
  
  def printCentroids() {
    for(i <- K)
      println(centroids(i))
  }
  
  def getClusterProfileMatrix(): DenseMatrix = {
    val profileList = centroids.toList.flatten
    val profileMatrix = new DenseMatrix(profileList.toArray, k, m.colSize, true)
    profileMatrix
  }

  def getClusterAsignmentMatrix(): DenseMatrix = {
    val assignmentList = Rows.map(r => K.map(i => if (assignments(r) == i) 1 else 0).toList).toList    
    val assignMatrix = new DenseMatrix(assignmentList.flatten.toArray, m.rowSize, k, true)    
    assignMatrix
  }
}