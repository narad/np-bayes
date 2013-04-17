package npbayes

/**
 * taken from Percy Liangs TEA utilities
 */
class SubArray[A](private val array:Array[A], private val start:Int, private val end:Int) extends RandomAccessSeq[A] {
  /**
   * does a content-based hash
   */
  override def hashCode = {
    var sum = 0
    var i = start
    while (i < end) {
      sum = (sum * 29 + array(i).hashCode)
      i += 1
    }
    sum
  }
  override def equals(_that:Any) : Boolean = {
    _that match {
      case that : SubArray[_] =>
        if (this.length == that.length) {
          var i = this.start
          var j = that.start
          while (i < this.end) {
            if (!(this.array(i) equals that.array(j)))
              return false
            i += 1
            j += 1
          }
          true
        }
        else
          false
      case _ => false
    }
  }
  override def length = end-start
  override def apply(i:Int) = {
    assume (i >= 0 && i < length)
    array(start+i)
  }
  override def toString = {
    val buf = new StringBuilder
    buf += '['
    var i = start
    while (i<end) {
      if (i > start) buf += ','
      buf.append(array(i).toString)
      i += 1
    }
    buf += ']'
    buf.toString
  }
}

class WordType(private val array:Array[Int],private val start:Int, private val end:Int, val finalSeg:Int = -1) extends 
	SubArray[Int](array,start,end) {
  override def length = {
    if (finalSeg == -1)
      end-start
    else
      end-start+1
  }
  override def apply(i:Int) = {
    assume(i>=0 && i<length)
    if (i==length-1)
      if (finalSeg == -1)
        array(start+i)
      else
        finalSeg
    else
      array(start+i)
  }
  
  def lastSeg: Int =
    array(start+length-1)
  
  def allButLast: WordType = {
    if (finalSeg == -1)
    	new WordType(array,start,end-1)
    else
      new WordType(array,start,end)
  }
}

package object wordseg {
	val DEBUG = false /** makes everything slow because of checks!! */
	/*
	 * Sharon Goldwater's annealing scheme
	 */
	def annealTemperature(startTemp: Double, annealIters: Int, stopTemp: Double = 1)(iter: Int) = {
	  def inner(iteration: Int) =
	    if (iteration >= annealIters)
	    	 stopTemp
	    else {
	    	val bin: Int = (9*iteration)/annealIters+1
      		(10.0/bin-1)*(startTemp-stopTemp)/9.0 + stopTemp
	    }
      inner(iter)
	}
	
	def annealTemperature(iter: Int) = {
	  def inner(iteration: Int) =
	    if (iteration >= 7000)
	    	 1
	    else {
	    	val bin: Int = (9*iteration)/7000+1
      		(10.0/bin-1)*(10-1)/9.0 + 1
	    }
      inner(iter)
	}
}