package srf.clustering

import srf.matrix.DenseMatrix

trait KMeansTrait {

  def getFootruleDist(v1: List[Int], v2: List[Int]): Long = {
    
    if (v1.length != v2.length) {
      throw new Exception("Two vectors do not have the same length")
    } 
    
    val dist = (0 until v1.length).map(i => java.lang.Math.abs(v1(i).toLong - v2(i).toLong)).sum    
    dist
  }
  
  def getSpearmanDist(v1: List[Int], v2: List[Int]): Long = {
    
    if (v1.length != v2.length) {
      throw new Exception("Two vectors do not have the same length")
    }
    
    val dist = (0 until v1.length).map(i => (v1(i).toLong - v2(i).toLong) * (v1(i).toLong - v2(i).toLong)).sum    
    dist
  }
  
  def getKernelDist(v1: List[Int], v2: List[Int], t: Double): Double = {
    
    /**
     * d(v1, v2) = exp(-EuclideanDist(v1,v2)/(t))
     * 
     * t: temperature value 
     * 
     */
    
    if (v1.length != v2.length) {
      throw new Exception("Two vectors do not have the same length")
    }
    
    val euclideanDist = (0 until v1.length).map(i => (v1(i).toLong - v2(i).toLong) * (v1(i).toLong - v2(i).toLong)).sum
    java.lang.Math.exp(-euclideanDist/t)
    
  }
  
  def getKNearestNeighbours(v: List[Int], m: DenseMatrix, k: Int): List[Int] = {
    
    /**
     * Given a rank vector v, find its k nearest neighbours (rows)
     * 
     */
    if (v.length != m.colSize) {
      throw new Exception("Not at the same size")
    }
    
    val distances = (0 until m.rowSize).map(r => this.getSpearmanDist(v, m.getRowVector(r)))
    val sortedDistances = distances.sorted
    val neighbours = (0 until k).map(i => distances.indexOf(sortedDistances(i))).toList
    neighbours
  }
  
  // randomize assignments of instances to a predefined a number of clusters
  def randomizeAssignments(): List[Int] 
  
  // assign instances to clusters
  def setClusterAssignments(assigns: List[Int])
  
  def updateCentroids()
  def updateAssignments(): Double
  def collectClusterMember(): List[List[Int]]
  def printClusterMember()
  def printWelcomeMsg()
  def printCentroids()
  
  
  // applying k-means nRepeats times
  def doClustering(nRepeats: Int) = {
    
    for (i <- 0 until nRepeats) {
      updateCentroids()
      updateAssignments()
    }
    
  }
  
  // applying k-means until converged
  def doClustering(): Double = {
    
    val maxIterations = 100
    val maxDiferences = 0.00001
    
    var n = 0
    var oldScore = 0.0
    var dif = 1.0
    
    val startTime = System.currentTimeMillis()
    
    printWelcomeMsg
    print("Iterations: ")
    while ((dif > maxDiferences) && (n < maxIterations)) {
      
      if (n == 0) {
        print (n+1)
      } else {
        print(", " + (n+1))
      }
      //println("** Before updating centroids:")
      //println("+ Centroids: ")
      //printCentroids()
      //println("+ Cluster members: ")
      //printClusterMember()
      
      updateCentroids()
      
      //println("** After updating centroids:")
     // println("+ Centroids: ")
      //printCentroids()
      //println("+ Cluster members: ")
      //printClusterMember()
      
      
      val newScore = updateAssignments()
      
      if (newScore == 0.0) {
        dif = 0.0
      } else {
        dif = (newScore - oldScore)/oldScore
      }
      
      oldScore = newScore
      n = n + 1  
      
    }
    
    val endTime = System.currentTimeMillis()
    val runTime = (endTime - startTime)/1000
    println("\nTerminated after " + n + " iterations (" + runTime + "s)")
    
    return oldScore
  }
  
  
  
}