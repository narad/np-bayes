package npbayes.wordseg.data

import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedList
import scala.collection.immutable.BitSet
import com.google.common.collect.HashBiMap

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

object PhonemeFeatureMap {
  val pmap: HashMap[SegmentType,BitSet] = new HashMap
  val fmapNtoS: HashMap[Int,String] = new HashMap
  val fmapStoN: HashMap[String,Int] = new HashMap


  var isInit: Boolean = false
  
  def nFeatures = {
//    assert(isInit)
    fmapStoN.size
  }
  
  def apply(phone: Int) = pmap(phone)
  
  def init(fName: String) = {
    if (isInit)
      throw new Error("Can only initialize the PhonemeFeatureMap once.")
    
    /**
     * first line specifies vocabulary
     */
    var firstLine = true
    var fNumber: Int = 0
    for (l <- scala.io.Source.fromFile(fName).getLines) {
      if (firstLine) {
        val features = l.stripMargin.stripLineEnd.split(" ")
        for (feat <- features) {
          fmapNtoS(fNumber)=feat
          fmapStoN(feat)=fNumber
          fNumber+=1
        }
        firstLine=false
      } else {
        val fVector = new scala.collection.mutable.BitSet()
        val features = l.stripMargin.stripLineEnd.split(" ")
        val phone = features(0)
        var i=1
        while (i<features.size) {
          fVector+=fmapStoN(features(i))
          i+=1
        }
        pmap(SymbolSegTable(phone))= BitSet.empty++fVector
      }
   }
	
  }
}
