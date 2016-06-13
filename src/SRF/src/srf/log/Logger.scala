package srf.log

import java.io._
import srf.matrix._

class Logger {
  
  def log(m: DenseMatrix, filename: String, delimiter: String) {    
    m.saveMatrix(filename, delimiter)
  }
  
  def log(s: String, filename: String, bAppend: Boolean) {
    if (bAppend) {
      val fw = new FileWriter(filename, true)
      try {      
    	fw.write(s + "\n")
      } catch {
        case e: Exception => println("IOException: " + e)
      } finally {
        fw.close()
      }
    } else {
      val fw = new FileWriter(filename, false)
      try {      
    	fw.write(s + "\n")
      } catch {
        case e: Exception => println("IOException: " + e)
      } finally {
        fw.close()
      }
    }
  }
  
  def log(n: Long, filename: String, bAppend: Boolean) {
    if (bAppend) {
      val fw = new FileWriter(filename, true)
      try {      
    	fw.write(n + "\n")
      } catch {
        case e: Exception => println("IOException: " + e)
      } finally {
        fw.close()
      }
    } else {
      val fw = new FileWriter(filename, false)
      try {      
    	fw.write(n + "\n")
      } catch {
        case e: Exception => println("IOException: " + e)
      } finally {
        fw.close()
      }
    }
  }
  
  

  def createDirectory (dirPath: String) {
    val file = new File(dirPath)
    
    if (file.exists()) {
      try {
        val filenames = file.list()        
        filenames.foreach(f => ((new File(dirPath + f)).delete()))
        file.delete()
        
      } catch {
        case e: IOException => println("IOException: " + e)
      }
    }
    
    try {
      file.mkdirs()
    } catch {
      case e: IOException => println("IOException: " + e)
    }
  }  
  
}