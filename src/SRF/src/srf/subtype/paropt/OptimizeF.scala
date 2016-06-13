package srf.subtype.paropt

import oscar.linprog.modeling._
import oscar.linprog._
import oscar.algebra._
import srf.matrix._
import srf.utils._

/**
 * Cancer subtyping using RMF
 * 
 * Optimize F given C
 * 
 * @author Thanh Le Van thanh.levan@cs.kuleuven.be
 * @Date December 30, 2015
 */

class OptimizeF (mutMatrix: DenseMatrix,// mutation matrix 
				rankedDif: DenseMatrix, // ranked diffusion matrix
				rankedExp: DenseMatrix, // ranked expression matrix
				factorisedDifC: DenseMatrix, 	// the factorised matrix C for ranked diffusion
				factorisedExpC: DenseMatrix,
				k: Int, 				// number of subtypes
				dTheta: Int, 			// ranked diffusion threshold 
				eTheta: Int, 			// ranked expression threshold
				beta: Double, 			// rank balancing score
				bDebug: Boolean) {
  
  def runOptimizeColumnF(colId: Int, 				// column id				
				rankedDifColumnValue: List[Int],// values of the row mutRowId-th in the ranked diffusion matrix
				rankedExpColumnValue: List[Int],//				 
				factorisedDifC: DenseMatrix, 	// the factorised matrix C for ranked diffusion
				factorisedExpC: DenseMatrix,	// the factorised matrix C for ranked expression
				k: Int, 						// number of subtypes				 
				dTheta: Int, 					// ranked diffusion threshold
				eTheta: Int,					// ranked expression threshold
				beta: Double, 					// rank balancing score
				bDebug: Boolean): OptimalRowModel = {
    
    val colFOptimizer = new OptimizeColF(colId, 
    									rankedDifColumnValue,
    									rankedExpColumnValue,
    									factorisedDifC,
    									factorisedExpC,
    									k,
    									dTheta,
    									eTheta,
    									beta,
    									bDebug)
    colFOptimizer.optimizeColAssignment
  }

  def optimize(name: String): SubtypeModel = {
      
	  val subtypeModel = new SubtypeModel(name, rankedDif.rowSize, rankedExp.rowSize, rankedExp.colSize, k)	  
	  subtypeModel.updateDifC(factorisedDifC)
	  subtypeModel.updateExpC(factorisedExpC)
	 	  
	  val startTime = System.currentTimeMillis()
	  
	  if (bDebug) {
	    println("** Optimize matrix F given the assignments of matrix C1 & C2")
	    println("dTheta = " + dTheta)
	    println("eTheta = " + eTheta)
		println("Mutation matrix M: " + mutMatrix.rowSize + "x" + mutMatrix.colSize)
		println("Ranked diffusion matrix D: " + rankedDif.rowSize + "x" + rankedDif.colSize)
		println("Ranked expression matrix E: " + rankedExp.rowSize + "x" + rankedExp.colSize)
		
	    println("Factorised difusion matrix C: ")
	    factorisedDifC.printMatrix
	    println("Factorised expression matrix C: ")
	    factorisedExpC.printMatrix
	  }
	  
	  val nMut = mutMatrix.rowSize
	  val nPat = mutMatrix.colSize
	  val nExp = rankedExp.rowSize
	  
	  val DRows = 0 until nMut
	  val ERows = 0 until nExp
	  val Cols = 0 until nPat
	  val K	= 0 until k
	  
	  // parallel collection for the columns
	  val parCols = Cols.toParArray
	  
	  println("Optimizing the factorised matrix F ")	  
	  val optimisedF = parCols.map(c => runOptimizeColumnF(c,
			  											rankedDif.getColumnVector(c),// expression row id				
			  											rankedExp.getColumnVector(c), // ranked expression row vector  
			  											factorisedDifC,
			  											factorisedExpC,			  											
			  											k,
			  											dTheta,
			  											eTheta, 			
			  											beta, 			
			  											bDebug))
	  
	  
	  
	  val optF = new DenseMatrix(k, nPat)
	  for (p <- Cols) {
	    val colF = optimisedF(p).getRowAssignments
	    for (r <- K) {
	      optF.setValue(r, p, colF(r))
	    }
	  }
	  
	  
	  val totalScore = optimisedF.map(op => op.getOptimalScore).sum.toLong
	  
	  subtypeModel.updateMatrixF(optF)	  
	  subtypeModel.updateObjValue(totalScore)
	  
	  val endTime = System.currentTimeMillis()	  
	  subtypeModel.updateRunTime(((endTime - startTime)/1000).toInt)
	  
	  if (bDebug) {
	    println("** Result: ")	    
	    println("Optimal score: " + totalScore)	    
	    println("Matrix F: ")
	    optF.printMatrix
	    println("Expression matrix C: ")
	    subtypeModel.getExpC.printMatrix
	    println("Diffusion matrix C: ")
	    subtypeModel.getDifC.printMatrix
	  } else {
	    println("*******************************************************")
	    println("Iteration id \t Objective value \t Running time")
	    println(name + "\t\t" + subtypeModel.getObjValue + "\t\t" + subtypeModel.getRunTime)
	    println("*******************************************************")	    
	  }	
	  
	  subtypeModel
  }
}