package npbayes.wordseg.data

import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedList

/**
 * Maps a SegmentType to a PhonemeClass identifier
 */
object PhonemeClassMap {
	val pmap: HashMap[SegmentType,Int] = new HashMap
	var isInit: Boolean = false
	
	def init(fName: String) = {
	  if (isInit)
	    throw new Error("Can only initialize the PhonemeClassMap once.")
	  for (l <- scala.io.Source.fromFile(fName).getLines) {
	    val args = l.stripMargin.stripLineEnd.split(" ")
        pmap.put(SymbolSegTable(args(1)), SymbolClassTable(args(0)))
	  }
	  isInit = true
	}
    
	def getClass(x: SegmentType) = {
	  if (!isInit)
	    throw new Error("Need to specify --phonmap parameter when using segment classes.")
	  pmap(x)
	}
	
    def apply(x: SegmentType) = {
      if (!isInit)
	    throw new Error("Need to specify --phonmap parameter when using segment classes.")
      PType(pmap(x))
    }
}

