package srf.subtype.paropt

import oscar.linprog.modeling._
import oscar.linprog._
import oscar.algebra._
import srf.matrix._
import srf.utils._

/**
 * Cancer subtyping using RMF
 * 
 * Optimize C given F
 * 
 * @author Thanh Le Van thanh.levan@cs.kuleuven.be
 * @Date December 30, 2015
 */

class OptimizeC (mutMatrix: DenseMatrix,// mutation matrix 
				rankedDif: DenseMatrix, // ranked diffusion matrix
				rankedExp: DenseMatrix, // ranked expression matrix
				matrixF: DenseMatrix, 	// the shared factorisation matrix F
				nReqMutations: Int,		// number of the required mutations
				k: Int, 				// number of subtypes
				dTheta: Int, 			// ranked diffusion threshold 
				eTheta: Int, 			// ranked expression threshold
				beta: Double, 			// rank balancing score
				bDebug: Boolean) {
  
  def runOptimizeExpRowC(expRowId: Int,				
				rankedExpRowValue: List[Int], 
				matrixF: DenseMatrix, 	
				k: Int, 								 
				eTheta: Int, 			
				beta: Double, 			
				bDebug: Boolean): OptimalRowModel = {
    
    val expRowOptimizer = new OptimizeExpRowC(expRowId, rankedExpRowValue, matrixF, k, eTheta, beta, bDebug)
    expRowOptimizer.optimizeRowAssignment
  }
  
  
  def runOptimizeDifRowC(mutRowId: Int,		  // mutation row id		
				rankedDifRowValue: List[Int], // values of the row mutRowId-th in the ranked diffusion matrix
				mutRowValue: List[Int], 	  // the binary mutation vector, which is the row mutRowId-th in the mutation matrix 
				matrixF: DenseMatrix, 		  // the shared factorisation matrix F
				nReqMuts: Int,
				k: Int, 					  // number of subtypes				 
				dTheta: Int, 				  // ranked expression threshold
				beta: Double, 				  // rank balancing score
				bDebug: Boolean): OptimalRowModel = {
    
    val difRowOptimizer = new OptimizeDifRowC(mutRowId, rankedDifRowValue, mutRowValue, matrixF, nReqMuts, k, dTheta, beta, bDebug)
    difRowOptimizer.optimizeRowAssignment    
  }
  

  def optimize(name: String): SubtypeModel = {
      
	  val subtypeModel = new SubtypeModel(name, rankedDif.rowSize, rankedExp.rowSize, rankedExp.colSize, k)	  
	  subtypeModel.updateMatrixF(matrixF)
	 	  
	  val startTime = System.currentTimeMillis()
	  
	  if (bDebug) {
	    println("** Optimize matrix C1 and C2 given the assignments of matrix F")
	    println("dTheta = " + dTheta)
	    println("eTheta = " + eTheta)
		println("Mutation matrix M: " + mutMatrix.rowSize + "x" + mutMatrix.colSize)
		println("Ranked diffusion matrix D: " + rankedDif.rowSize + "x" + rankedDif.colSize)
		println("Ranked expression matrix E: " + rankedExp.rowSize + "x" + rankedExp.colSize)
		
	    println("Matrix F: ")
	    matrixF.printMatrix
	  }
	  
	  val nMut = mutMatrix.rowSize
	  val nPat = mutMatrix.colSize
	  val nExp = rankedExp.rowSize
	  
	  val DRows = 0 until nMut
	  val ERows = 0 until nExp
	  val Cols = 0 until nPat  
	  val K	= 0 until k
	  
	  // parallel collection for DRows & ERows
	  val parDRows = DRows.toParArray
	  val parERows = ERows.toParArray
	  
	  println("Optimizing the factorised matrix C for ranked expression ...")	  
	  val optimisedExpC = parERows.map(r => runOptimizeExpRowC(r, 						   // expression row id				
			  												rankedExp.getRowVector(r), // ranked expression row vector  
			  												matrixF, 	
			  												k, 								 
			  												eTheta, 			
			  												beta, 			
			  												bDebug))
	  
	  println("Optimizing the factorised matrix C for ranked diffusion ...")
	  val optimisedDifC = parDRows.map(r => runOptimizeDifRowC(r,				
															rankedDif.getRowVector(r), // values of the row mutRowId-th in the ranked diffusion matrix
															mutMatrix.getRowVector(r), // the binary mutation vector, which is the row mutRowId-th in the mutation matrix 
															matrixF, 	// the shared factorisation matrix F
															nReqMutations,
															k, 				// number of subtypes				 
															dTheta, 			// ranked expression threshold
															beta, 			// rank balancing score
															bDebug))
	  
	  val optExpC = new DenseMatrix(optimisedExpC.map(op => op.getRowAssignments).flatten.toArray, nExp, k, true)
	  val optDifC = new DenseMatrix(optimisedDifC.map(op => op.getRowAssignments).flatten.toArray, nMut, k, true)
	  val expScore = optimisedExpC.map(op => beta * op.getOptimalScore).sum.toLong
	  val difScore = optimisedDifC.map(op => op.getOptimalScore).sum.toLong
	  val totalScore = difScore + expScore
	  
	  subtypeModel.updateExpC(optExpC)
	  subtypeModel.updateDifC(optDifC)
	  subtypeModel.updateObjValue(totalScore)
	  
	  val endTime = System.currentTimeMillis()	  
	  subtypeModel.updateRunTime(((endTime - startTime)/1000).toInt)
	  
	  if (bDebug) {
	    println("** Result: ")	    
	    println("Optimal score: " + totalScore)
	    println("Expression matrix C: ")
	    optExpC.printMatrix
	    println("Diffusion matrix C: ")
	    optDifC.printMatrix
	    println("Matrix F: ")
	    matrixF.printMatrix	       
	  } else {
	    println("*******************************************************")
	    println("Iteration id \t Objective value \t Running time")
	    println(name + "\t\t" + subtypeModel.getObjValue + "\t\t" + subtypeModel.getRunTime)
	    println("*******************************************************")	    
	  }	
	  
	  subtypeModel
  }
}