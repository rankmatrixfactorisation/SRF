package srf.utils

import srf.matrix._
import java.io._

class RankedFactorisationModel(name: String, m: Int, n: Int, k: Int) {

  val C = new DenseMatrix(m, k)
  val F = new DenseMatrix(k, n)
  val objValue = new Array[Long](1)
  val runTime  = new Array[Int](1) // in ms
  
  def updateRunTime(r: Int) {
    runTime(0) = r
  }
  
  def getRunTime(): Int = runTime(0)
  
  def updateObjValue(v: Long) {
    objValue(0) = v
  }
  
  def getObjValue() = objValue(0)
  
  def updateMatrixC (m: DenseMatrix) {
    
    if ((m.rowSize != C.rowSize) || (m.colSize != C.colSize)) {
      throw new Exception("Matrix dimensions mismatched.")
    }
    
    for (i <- 0 until m.rowSize) {
      for (j <- 0 until m.colSize) {
        C.setValue(i, j, m.at(i,j))
      }
    }
  }
  
  def updateMatrixF (m: DenseMatrix) {
    
    if ((m.rowSize != F.rowSize) || (m.colSize != F.colSize)) {
      throw new Exception("Matrix dimensions mismatched.")
    }
    
    for (i <- 0 until m.rowSize) {
      for (j <- 0 until m.colSize) {
        F.setValue(i, j, m.at(i,j))
      }
    }
  }
  
  def getMatrixC = C
  def getMatrixF = F
  
  def save(CFileName: String, FFileName: String, OFileName: String, delimiter: String) {
    F.saveMatrix(FFileName, delimiter)
    C.saveMatrix(CFileName, delimiter)
    
    // save objective value
    val writer = new FileWriter(OFileName, true) // append = true
    try {
      writer.write(this.getObjValue.toString + "\t" + runTime(0).toString() + "\n")
      writer.close()
    } catch {
      case e: Exception => println("IOException: " + e)
    } finally {
      writer.close()
    }
        
  }
  
}