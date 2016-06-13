package srf.utils

class OptimalRowModel (rowId: Int, k: Int) {

  val rowAssignments = new Array[Int](k)
  val score = new Array[Double](1)
  
  def getRowAssignments = rowAssignments.toList
  def getOptimalScore = score(0)
  
   
  def setRowAssignment(v: List[Int]) {
    if (v.length != k) {
      throw new Exception("Row vector mismatched")
    }
    
    for (i <- 0 until v.length) {
      rowAssignments(i) = v(i)
    }
  }
  
  def setOptimalScore(s: Double) {
    score(0) = s
  }
  
  def getRowId = rowId
  
}