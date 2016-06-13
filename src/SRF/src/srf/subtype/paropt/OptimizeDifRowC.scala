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


class OptimizeDifRowC (mutRowId: Int,				
				rankedDifRowValue: List[Int], // values of the row mutRowId-th in the ranked diffusion matrix
				mutRowValue: List[Int], // the binary mutation vector, which is the row mutRowId-th in the mutation matrix 
				matrixF: DenseMatrix, 	// the shared factorisation matrix F
				nReqMutations: Int,
				k: Int, 				// number of subtypes				 
				dTheta: Int, 			// ranked expression threshold
				beta: Double, 			// rank balancing score
				bDebug: Boolean) {
    
  val nPat = matrixF.colSize()
  val Cols = 0 until nPat
  val K = 0 until k
  
  
  def optimizeRowAssignment(): OptimalRowModel = {
    
    val mip = new MIPSolver(LPSolverLib.gurobi)
    
    
    val optimalRowModel = new OptimalRowModel(mutRowId, k)
  
	val difC = Array.tabulate(k)(c => new MIPIntVar(mip, "c-row-col" + (mutRowId, c), 0 to 1))
	val difA = Array.tabulate(nPat)(c => new MIPIntVar(mip, "a-row-col" + (mutRowId, c), 0 to 1))
	  
	// scoring function
	val score = sum(Cols) {c => rankedDifRowValue(c) * difA(c) - dTheta * difA(c)}
	  
	// coverage constraints for ranked expression	
	for (j <- Cols) {
	  mip.add( difA(j) <= sum(K) {t => difC(t) * matrixF.at(t, j) } )
	  mip.add( (difA(j)*nPat) >= sum(K) {t => difC(t) * matrixF.at(t, j)} )
	}
	
	// mutation constraints	
	for (j <- K) {	    
	  //mip.add( difC(j) <= sum(Cols) {p => mutRowValue(p) * matrixF.at(j, p)} )
	  val minReqs = (0 until nReqMutations).map(nr => -nr + Cols.map(p => mutRowValue(p) * matrixF.at(j,p)).sum)
	  val prodVal = minReqs.product
	  mip.add( difC(j) <= prodVal)	  
	}	    
	  
	// solving the optimisation
	mip.maximize(score)	  	  
	mip.start()
	
	// update result
	optimalRowModel.setOptimalScore(score.value.get)
	val assignments = K.map(i => difC(i).value.get.toInt)
	optimalRowModel.setRowAssignment(assignments.toList)  
	
	if (bDebug) {
	  println("Mutation row id = " + mutRowId + ": " + assignments.toList)
	}
	
	// release mip solver
	mip.release	
	
	// return final result
	optimalRowModel
  }
  
}