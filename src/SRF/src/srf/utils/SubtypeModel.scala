package srf.utils


import srf.matrix._
import srf.log._
import java.io._


/**
 * Store the factorisation model for the cancer subtyping problem
 * 
 * Created on December 24, 2015
 * @author Thanh Le Van thanh.levan@cs.kuleuven.be
 */

class SubtypeModel(name: String, nMut: Int, nExp: Int, nPat: Int, k: Int) {
  
  val DifC = new DenseMatrix(nMut, k)
  val ExpC = new DenseMatrix(nExp, k)
  val F = new DenseMatrix(k, nPat)
  
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
  
  def updateDifC (m: DenseMatrix) {
    
    if ((m.rowSize != DifC.rowSize) || (m.colSize != DifC.colSize)) {
      throw new Exception("Matrix dimensions mismatched.")
    }
    
    for (i <- 0 until m.rowSize) {
      for (j <- 0 until m.colSize) {
        DifC.setValue(i, j, m.at(i,j))
      }
    }
  }
  
  def updateExpC (m: DenseMatrix) {
    
    if ((m.rowSize != ExpC.rowSize) || (m.colSize != ExpC.colSize)) {
      throw new Exception("Matrix dimensions mismatched.")
    }
    
    for (i <- 0 until m.rowSize) {
      for (j <- 0 until m.colSize) {
        ExpC.setValue(i, j, m.at(i,j))
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
  
  def getDifC = DifC
  def getExpC = ExpC
  def getMatrixF = F
  
  def save(DifCFileName: String, ExpCFileName: String, FFileName: String, OFileName: String, delimiter: String) {
    F.saveMatrix(FFileName, delimiter)
    DifC.saveMatrix(DifCFileName, delimiter)
    ExpC.saveMatrix(ExpCFileName, delimiter)
    
    // save objective value
    val writer = new PrintWriter(new File(OFileName))
    writer.write(this.getObjValue.toString)
    writer.close()
  }
  
  def save(modelId: Int, saveDir: String, delimiter: String) {
    val fFile    = if (modelId < 0) saveDir + "F.txt" else saveDir + "F_" + modelId.toString + ".txt" 
    val expCFile = if (modelId < 0) saveDir + "expC.txt" else saveDir + "expC_" + modelId.toString + ".txt"
    val difCFile = if (modelId < 0) saveDir + "difC.txt" else saveDir + "difC_" + modelId.toString + ".txt"
    val objFile  = if (modelId < 0) saveDir + "score.txt" else saveDir + "score_" + modelId.toString + ".txt" 
    val timeFile = if (modelId < 0) saveDir + "time.txt" else saveDir + "time_" + modelId.toString + ".txt"
    
    val logger = new Logger()
    logger.log(this.getObjValue, objFile, false)
    logger.log(this.getRunTime, timeFile, false)
    F.saveMatrix(fFile, delimiter)
    DifC.saveMatrix(difCFile, delimiter)
    ExpC.saveMatrix(expCFile, delimiter)
  }
  
  def printSubtypeResult() {
    
    println("* Objective score = " + this.getObjValue)
    println
    for (s <- 0 until k) {
      val colVector = F.getRowVector(s)
      val mutVector = DifC.getColumnVector(s)
      val expVector = ExpC.getColumnVector(s)
      
      val patients = (0 until nPat).filter(p => colVector(p) > 0)
      val mutations = (0 until nMut).filter(m => mutVector(m) > 0)
      val expressions = (0 until nExp).filter(e => expVector(e) > 0)
            
      println("* Subtype " + s + " - nPatients = " + patients.length + " - nMutations = " + mutations.length + " - nExpressions = " + expressions.length)      
      println("Patients: " + patients)
      println("Mutations: " + mutations)      
      println("Expressions: " + expressions)
      println
    }    
  }
  
  def saveSubtypeResult(saveDir: String) {
    
    val logger = new Logger()
    
    for (s <- 0 until k) {
      val colVector = F.getRowVector(s)
      val mutVector = DifC.getColumnVector(s)
      val expVector = ExpC.getColumnVector(s)
      
      val patients = (0 until nPat).filter(p => colVector(p) > 0)
      val mutations = (0 until nMut).filter(m => mutVector(m) > 0)
      val expressions = (0 until nExp).filter(e => expVector(e) > 0)
      
      val pMatrix = new DenseMatrix(patients.toArray, 1, patients.length, true)
      val mMatrix = new DenseMatrix(mutations.toArray, 1, mutations.length, true)
      val eMatrix = new DenseMatrix(expressions.toArray, 1, expressions.length, true)
      
      val workDir = saveDir + (s+1)
      
      logger.createDirectory(workDir)
      logger.log(pMatrix, workDir + "/cols.txt", "\t")
      logger.log(mMatrix, workDir + "/drows.txt", "\t")
      logger.log(eMatrix, workDir + "/erows.txt", "\t")
    
    }
    
  }
  
}