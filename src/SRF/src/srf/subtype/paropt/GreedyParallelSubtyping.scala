package srf.subtype.paropt


import util.Random.shuffle 
import java.io._
import srf.matrix._
import srf.utils._
import srf.clustering._
import srf.log._


class GreedyParallelSubtyping (mutFileName: String, 
						rankedDifFileName: String, 
						rankedExpFileName: String,
						initMatrixFFileName: String, // if this string is not empty, the matrix F is initialized to this one
						maxRankedDifValue: Int,		// max ranked diffusion value 
						maxRankedExpValue: Int, // max ranked expression value
						nReqMutations: Int, // number of the required mutations
						k: Int, 
						dTheta: Double, 
						eTheta: Double,
						beta: Double,						
						workDir: String,
						bLog: Boolean, 
						bDebug: Boolean) {
  
  val mutMatrix = new DenseMatrix(mutFileName, "\t")
  val rankedDifMatrix  = new DenseMatrix(rankedDifFileName, "\t")
  val rankedExpMatrix  = new DenseMatrix(rankedExpFileName, "\t")
  
  val initMatrixF = if (!initMatrixFFileName.isEmpty) new DenseMatrix(initMatrixFFileName, "\t") else new DenseMatrix(1,1)
		
  val nMut = rankedDifMatrix.rowSize
  val nPat = rankedDifMatrix.colSize
  val nExp = rankedExpMatrix.rowSize

  val maxExpValue = if (maxRankedExpValue == 0) rankedExpMatrix.colSize else maxRankedExpValue
  val iExpTheta = java.lang.Math.round(eTheta*maxExpValue).toInt
  val maxDifValue = if (maxRankedDifValue == 0) rankedDifMatrix.rowSize else maxRankedDifValue
  val iDifTheta = java.lang.Math.round(dTheta*maxDifValue).toInt
  
   
  println("iExpTheta = " + iExpTheta)
  println("iDifTheta = " + iDifTheta)
  
  val logger = new Logger()
    
  def repeatDecomposition(nRepeat: Int) {
    
    val startTime = System.currentTimeMillis()
    
    val candModels = (0 until nRepeat).map(n => decompose(n, bLog))
    val sortedModels = candModels.sortBy(m => -m.getObjValue) //get maximal value
    val bestModel = candModels(candModels.indexOf(sortedModels(0)))
    
    val endTime = System.currentTimeMillis()
    val runTime = ((endTime - startTime)/1000).toInt
    bestModel.updateRunTime(runTime)
    
    println
    println("*****************************************************")
    println("* The best model after " + nRepeat + " iterations " + " in " + runTime + "(s)\t\t*")
    println("**********************  *******************************")
    println
    bestModel.printSubtypeResult
    
    bestModel.saveSubtypeResult(workDir)
    bestModel.save(-1, workDir, "\t")
  }
  
  /**
   * decompose starts with a randomized matrix F
   */
  def decompose(name: Int, bLog: Boolean): SubtypeModel = {
    
    val localStartTime = System.currentTimeMillis()
    val curWorkDir = workDir + "r_" + name + "/"
    if (bLog) {
      println("bLog is true")
      logger.createDirectory(curWorkDir)
    } else {
      println("bLog is false")
    }
  	
	val matrixF    = new DenseMatrix(k, nPat) 
	val matrixDifC = new DenseMatrix(nMut, k)
    val matrixExpC = new DenseMatrix(nExp, k)
	
	matrixF.copy(initMatrixF)
	println("** Matrix F is initialised to :")
	printSubtypePatientsFromMatrixF(matrixF)
	println("******************************")
	
	
	var oldScore = -1000000000.0
	var bStop = false
	var steps = 0
	val maxSteps = 100
	val maxImprovedRatio = 0.0001
	  
	while (!bStop && (steps < maxSteps)) {
	  println("**********************************************************")
	  println("*Iteration " + steps)
	  
	  ///////////////////////////////////////////////
	  // given F, find Cs	    
	  ///////////////////////////////////////////////
	  val cOptimizer = new OptimizeC(mutMatrix, 
	    							   rankedDifMatrix,
	    							   rankedExpMatrix,
	    							   matrixF,
	    							   nReqMutations,
	    							   k,
	    							   iDifTheta,
	    							   iExpTheta,
	    							   beta,
	    							   bDebug)
	  val newModelC = cOptimizer.optimize(name + "." + steps)
	  val newScore = newModelC.getObjValue
	  matrixDifC.copy(newModelC.getDifC)
	  matrixExpC.copy(newModelC.getExpC)
	  if (bLog) {	     
	      newModelC.save(steps, curWorkDir, "\t")
	  }
	  
	  // check convergence
	  // Note that we only check the convergent criterion in the step
	  // that optimizes C given F. It is because this step imposes the  
	  // mutations constraints (this constraint is relaxed for the other step).
	  
	  val improvedScore = scala.math.abs((newScore - oldScore)/oldScore)	    
	  if ( improvedScore <= maxImprovedRatio) {
	    bStop = true
	  } else {
	    oldScore = newScore
	  }
	  	  
	  steps = steps + 1
	  
	  if (!bStop) {
 	    ///////////////////////////////////////////////
	    // given C, find F
	    ///////////////////////////////////////////////  
	    val fOptimizer = new OptimizeF(mutMatrix, 
	    							   rankedDifMatrix,
	    							   rankedExpMatrix,
	    							   matrixDifC,
	    							   matrixExpC,
	    							   k,
	    							   iDifTheta,
	    							   iExpTheta,
	    							   beta,
	    							   bDebug)
	    
	    val newModel = fOptimizer.optimize(name + "." + steps)
	    val newScore = newModel.getObjValue
	    matrixF.copy(newModel.getMatrixF)
	    	    
	    if (bLog) {
	      newModel.save(steps, curWorkDir, "\t")
	    }
	    
	    steps = steps + 1
	  } // end if(!bStop)
	  
	} // end while
    
	val finalModel = new SubtypeModel("", nMut, nExp, nPat, k)
    finalModel.updateDifC(matrixDifC)
    finalModel.updateExpC(matrixExpC)
    finalModel.updateMatrixF(matrixF)
    finalModel.updateObjValue(oldScore.toLong)
    val localRunTime = ((System.currentTimeMillis() - localStartTime)/1000).toInt
    finalModel.updateRunTime(localRunTime)
    
    if (bLog) {
      finalModel.save(-1, curWorkDir, "\t") // -1 to remove modelId
      finalModel.saveSubtypeResult(curWorkDir)
    }
    
    println("*********Final result " + name + " ************")
    finalModel.printSubtypeResult
    
    finalModel  
  }
   
 
  def printSubtypePatientsFromMatrixF(f: DenseMatrix) = {
    
    val nSubtypes = f.rowSize
     
    for(s <- 0 until nSubtypes) {
      val subtypeVector = f.getRowVector(s)
      val subtypePatients = (0 until f.colSize).filter(c => subtypeVector(c) == 1)
      println("Group 1")
      println(subtypePatients)
    }
  }

}