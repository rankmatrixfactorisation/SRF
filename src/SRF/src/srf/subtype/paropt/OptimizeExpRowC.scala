package srf.subtype.paropt

import oscar.linprog.modeling._
import oscar.linprog._
import oscar.algebra._
import srf.matrix._
import srf.utils._

/**
 * Cancer subtyping using RMF
 * 
 * Optimize a single row of the factorised matrix C given F
 * 
 * @author Thanh Le Van thanh.levan@cs.kuleuven.be
 * @Date December 30, 2015
 */

class OptimizeExpRowC (expRowId: Int,				
				rankedExpRowValue: List[Int], // values of the row expRowId in the ranked expression matrix
				matrixF: DenseMatrix, 	// the shared factorisation matrix F
				k: Int, 				// number of subtypes				 
				eTheta: Int, 			// ranked expression threshold
				beta: Double, 			// rank balancing score
				bDebug: Boolean) {
    
  val nPat = matrixF.colSize()
  val Cols = 0 until nPat
  val K = 0 until k
    
  def optimizeRowAssignment(): OptimalRowModel = {
    
    val mip = new MIPSolver(LPSolverLib.gurobi)
    
    val optimalRowModel = new OptimalRowModel(expRowId, k)
  
	val expC = Array.tabulate(k)(c => new MIPIntVar(mip, "c-row-col" + (expRowId, c), 0 to 1))
	val expA = Array.tabulate(nPat)(c => new MIPIntVar(mip, "a-row-col" + (expRowId, c), 0 to 1))
	  
	// scoring function
	val score = sum(Cols) {c => rankedExpRowValue(c) * expA(c) - eTheta * expA(c)}
	  
	// coverage constraints for ranked expression	
	for (j <- Cols) {
	  mip.add( expA(j) <= sum(K) {t => expC(t) * matrixF.at(t, j) } )
	  mip.add( (expA(j)*nPat) >= sum(K) {t => expC(t) * matrixF.at(t, j)} )
	}
	  
	// solving the optimisation
	mip.maximize(score)	  	  
	mip.start()
	
	// update result
	optimalRowModel.setOptimalScore(score.value.get)
	val assignments = K.map(i => expC(i).value.get.toInt)
	
	optimalRowModel.setRowAssignment(assignments.toList)  
	
	if (bDebug) {
	  println("Exp row " + expRowId + ": " + assignments.toList)
	}
	
	
	// release mip solver
	mip.release	
	
	// return final result
	optimalRowModel
  }
  
}