package srf.clustering

import scala.collection.immutable.List
import util.Random.shuffle
import scala.util.Random
import srf.matrix._

class GeneralKMeans(m: DenseMatrix, k: Int, rankDistFunc: RankDistanceFunction.Value) extends KMeansTrait {
  
  // constructor using matrix filename
  def this(filename: String, k: Int, rankDistFunc: RankDistanceFunction.Value) {
    this(new DenseMatrix(filename, "\t"), k, rankDistFunc)
  }
  
  // class variables
  val K = 0 until k
  val Rows = 0 until m.rowSize
  val Cols = 0 until m.colSize
  
  val rand = new Random()
  var centroids = new Array[List[Int]](k)
  var assignments = new Array[Int](m.rowSize)
  
  // methods

  override def randomizeAssignments(): List[Int] = {
    val assigns = Rows.map(r => Random.nextInt(k))
    return (assigns.toList)
  }
  
  override def setClusterAssignments(assigns: List[Int]) = {
    Rows.map(r => assignments(r) = assigns(r))
  }
  
  def getDistancesToCentroids(): List[List[Long]] = {
    
    val rowDistances = rankDistFunc match {
      
      case RankDistanceFunction.footrule =>
        // perform parallel calculations 
    	Rows.toParArray.map(r => K.map(cluster => this.getFootruleDist(m.getRowVector(r), centroids(cluster))).toList).toList
    	
      case RankDistanceFunction.spearman =>
        // perform parallel calculations
        Rows.toParArray.map(r => K.map(cluster => this.getSpearmanDist(m.getRowVector(r), centroids(cluster))).toList).toList
    }
    
    return (rowDistances)
  }
  
  def updateCentroidKth(k: Int) = {
    
    val clusterMem = Rows.filter(r => assignments(r) == k)
    
    if (clusterMem.length > 0) {
      val colSum = Cols.map(c => clusterMem.map(r => m.at(r, c)).sum).toList
      val centroidK = colSum.map(c => java.lang.Math.round(c/clusterMem.length))
      centroids(k) = centroidK
      
    } else {
      centroids(k) = List.fill(Cols.length)(0)
    }
    
  }
  
  override def updateCentroids() {
    
    if (!isInitialized) {     
      val randomAssigns = randomizeAssignments()
      setClusterAssignments(randomAssigns)      
    }
    
    // update centroids
    K.toParArray.map(i => updateCentroidKth(i))  
    
  }
  
  def getRowDistanceToHerCentroid(distancesToCentroids: List[List[Long]]): IndexedSeq[Long] = {
    val currentDistances = Rows.map(r => distancesToCentroids(r)(assignments(r)))
    return currentDistances
  }
  
  override def updateAssignments(): Double = {
    
    // for each instance, calculate its distances to the k centroids
    val distancesToCentroids = getDistancesToCentroids()
    
    // calculate the score value given the current assignments and the current centriods    
    val currentDistances = getRowDistanceToHerCentroid(distancesToCentroids)//Rows.map(r => distancesToCentroids(r)(assignments(r)))
    
    // determine the new assignments for the instances
    val sortedRowDistances = distancesToCentroids.map(rowDists => rowDists.sorted)
    val newAssignments = Rows.map(r => distancesToCentroids(r).indexOf(sortedRowDistances(r)(0)))
    
    // update assignments    
    setClusterAssignments(newAssignments.toList)    
    
    // 
    return (currentDistances.sum)
  }
  
  def isInitialized() = if (assignments.sum > 0) true else false

  override def collectClusterMember(): List[List[Int]] = {
    val cMembers = K.map(i => Rows.filter(r => assignments(r) == i).toList).toList
    cMembers
  }
  
  def printClusterMember() {
    val clustMem = collectClusterMember
    clustMem.foreach(x => println(x))
  }
  
  def printWelcomeMsg() {
    println("KMeans clustering: " )
    println("Data matrix: " + m.rowSize + " x " + m.colSize )
  }
  
  def printCentroids() {
    for(i <- K) {
      println("Centroid " + i)
      println(centroids(i))
    }
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