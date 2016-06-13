package srf.subtype.paropt

import oscar.linprog.modeling._
import oscar.linprog._
import oscar.algebra._
import srf.matrix._
import srf.utils._

/**
 * Cancer subtyping using RMF
 * 
 * Optimize a single column of the factorised matrix F given the matrices Cs.
 * 
 * @author Thanh Le Van thanh.levan@cs.kuleuven.be
 * @Date January 4, 2015
 */

class OptimizeColF (colId: Int, 				// column id				
				rankedDifColumnValue: List[Int],// values of the row mutRowId-th in the ranked diffusion matrix
				rankedExpColumnValue: List[Int],//				 
				factorisedDifC: DenseMatrix, 	// the factorised matrix C for ranked diffusion
				factorisedExpC: DenseMatrix,	// the factorised matrix C for ranked expression
				k: Int, 						// number of subtypes				 
				dTheta: Int, 					// ranked diffusion threshold
				eTheta: Int,					// ranked expression threshold
				beta: Double, 					// rank balancing score
				bDebug: Boolean) {
  
  val nMut = rankedDifColumnValue.length
  val nExp = rankedExpColumnValue.length
  
  val DRows = 0 until nMut
  val ERows = 0 until nExp
  val K = 0 until k

  def optimizeColAssignment(): OptimalRowModel = {
    
    val mip = new MIPSolver(LPSolverLib.gurobi)
    
    val optimalRowModel = new OptimalRowModel(colId, k)

    // Column vector of the factorised matrix F
	val colVectorF = Array.tabulate(k)(c => new MIPIntVar(mip, "F-column-" + (colId, c), 0 to 1))
	
	// column covered vector for ranked expression
	val expA = Array.tabulate(nExp)(c => new MIPIntVar(mip, "a-row-col" + (c, colId), 0 to 1))
	// column covered vector for ranked diffusion
	val difA = Array.tabulate(nMut)(c => new MIPIntVar(mip, "a-row-col" + (c, colId), 0 to 1))   
	
	// scoring function
	val difScore = sum(DRows) {r => rankedDifColumnValue(r) * difA(r) - dTheta * difA(r)}
	val expScore = sum(ERows) {r => beta * (rankedExpColumnValue(r) * expA(r) - eTheta * expA(r))}
    
	val score = difScore + expScore
	
	// coverage constraints for ranked expression	
	for (j <- ERows) {
	  mip.add( expA(j) <= sum(K) {t => factorisedExpC.at(j, t) * colVectorF(t) } )
	  mip.add( (expA(j)*k) >= sum(K) {t => factorisedExpC.at(j, t) * colVectorF(t) } )
	}
	
	// coverage constraints for ranked diffusion
	for (j <- DRows) {
	  mip.add( difA(j) <= sum(K) {t => factorisedDifC.at(j, t) * colVectorF(t) } )
	  mip.add( (difA(j)*k) >= sum(K) {t => factorisedDifC.at(j, t) * colVectorF(t) } )
	}
	  
	// solving the optimisation
	mip.maximize(score)	  	  
	mip.start()
	
	// update result
	optimalRowModel.setOptimalScore(score.value.get)
	val assignments = K.map(i => colVectorF(i).value.get.toInt)
	
	optimalRowModel.setRowAssignment(assignments.toList)  
	
	if (bDebug) {
	  println("Column " + colId + ": " + assignments.toList)
	}
		
	// release mip solver
	mip.release	
	
	// return final result
	optimalRowModel
  }
}