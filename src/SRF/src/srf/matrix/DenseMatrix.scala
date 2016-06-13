package srf.matrix


/**
 * @author thanh
 *
 */

import scala.io.Source
import java.io._

class DenseMatrix(data: Array[Int], m: Int, n: Int, byRow: Boolean) {
      
  def this(filename: String, delimiter: String) {    
    this(Source.fromFile(filename).getLines().map(line => line.split(delimiter).map(_.toInt ).toArray).toArray.flatten,
        Source.fromFile(filename).getLines().size,
        Source.fromFile(filename).getLines().next.split(delimiter).map(_.toInt).length,
        true)
  }
  
  def this(m: Int, n: Int) {
    this(new Array[Int](m*n), m, n, true)
  }

  def rowSize(): Int = m//denseMatrix.size 
  
  def colSize(): Int = n //if(denseMatrix.size <=0 ) 0 else denseMatrix(0).size
    
  def at(i: Int, j: Int): Int = {
    if(i < 0 || i > rowSize)
      throw new IllegalArgumentException("row index is beyond the boundary")
    if(j < 0 || j > colSize)
      throw new IllegalArgumentException("column index is beyond the boundary")
    
    if (!byRow) {
      throw new Exception("byCol is not supported at the moment")
    }
    
    data(n*(i) + j)
  }
  
  /**
   * set value for (i,j)
   * 0-based indexes
   */
  def setValue(i: Int, j: Int, value: Int) = {
    
    if ((i >= m) || (j >= n)) {
      throw new Exception("Matrix index is out of range")
    }
    
    if (!byRow) {
      throw new Exception("byCol is not supported at the moment")
    }
    
    data(n*(i) + j) = value
  }
  
  def copy(s: DenseMatrix) {
    if ((m != s.rowSize) || (n != s.colSize)) {
      throw new Exception("source and destination matrices are not at the same size")
    } 
    
    for (r <- 0 until m) {
      for (c <- 0 until n) {
        this.setValue(r, c, s.at(r, c))
      }
    }
  }
  
  def printMatrix() = {    
    for(r <- 0 until m){
      for(c <- 0 until n) {
        print(this.at(r, c) + "\t")        
      }
      println()
    } 
  }
  
  def saveMatrix(filename: String, delimiter: String) {
    val writer = new PrintWriter(new File(filename))
    
    for (i <- 0 until rowSize) {
    	for (j <- 0 until colSize) {
    	  if (j < colSize - 1) { 
    		writer.write(this.at(i, j).toString + delimiter)
    	  } else {
    	    writer.write(this.at(i, j).toString)
    	  }
    	}    	
    	writer.write("\n")
    }
    
    writer.close()
  }
  
  def getRowVector(r: Int): List[Int] = {
    val row = (0 until n).map(c => this.at(r, c)).toList
    row
  }
  
  def getColumnVector(c: Int): List[Int] = {
    val col = (0 until m).map(r => this.at(r, c)).toList
    col
  }
  
  def transpose(): DenseMatrix = {
    val tMatrix = new DenseMatrix(this.colSize, this.rowSize)
    
    for ( i <- 0 until rowSize) {
      for (j <- 0 until colSize) {
        tMatrix.setValue(j, i, this.at(i, j))
      }
    }
    
    tMatrix
  }
  

}