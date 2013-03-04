package npbayes.wordseg.data

import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableList.Builder
import com.google.common.collect.HashBiMap

class Result(val tp: Double,val tr: Double, val bp: Double, val br: Double, val dp: Double, val dr: Double) {
  val tf = 2*tp*tr/(tp+tr)
  val bf = 2*bp*br/(bp+br)
  val df = 2*dp*dr/(dp+dr)
  
  override def toString =
    "P "+tp+" R "+tr+" F "+tf+" BP "+bp+" BR "+br+" BF "+bf+" DP "+dp+" DR "+dr+" DF "+df
} 


object SymbolTable {
  val mappingStringToSeg = HashBiMap.create[String,SegmentType]()
  def mappingSegToString = mappingStringToSeg.inverse
  var nextR: Int = 1
  def nSymbols = mappingStringToSeg.size
  def getNextR: Int = {
    nextR = (nextR+1)
    (nextR-1).toInt
  }
  
  def apply(x: String): SegmentType = {
    //println("retrieve Symbol: "+x)
    val res = mappingStringToSeg.get(x)
    if (res==0) {
      val newId=getNextR
      mappingStringToSeg.put(x, newId)
      newId
    } else
      res
  }
  
  def apply(x: SegmentType): String =
    mappingSegToString.get(x)
}

object `package` {
  type SegmentType = Int
  type WordType = ImmutableList[SegmentType]
  
  /**
   * convenience functions to build and display words
   */
  implicit def wToS (w: WordType): String = {
    val res = new StringBuilder
    for (i: Int <- (0 to w.size-1)){ 
      res.append(SymbolTable(w.get(i)))
    }
    res.result
  }
  
  implicit def sToW (s: String): WordType = {
    val res = new ImmutableList.Builder[SegmentType]
    for (c <- s.toList)
      res.add(SymbolTable(c.toString))
    res.build
  }

  def concat(w1: WordType, w2: WordType): WordType = 
    new ImmutableList.Builder[SegmentType].addAll(w1).addAll(w2).build()
  
  def suffix(w: WordType, suff: SegmentType) =
    new ImmutableList.Builder[SegmentType].addAll(w).add(suff).build()
  
  def prefix(pref: SegmentType, w: WordType) =
    new ImmutableList.Builder[SegmentType].add(pref).addAll(w).build()
  
  def segToWord(s: SegmentType): WordType =
    new ImmutableList.Builder[SegmentType].add(s).build()

} 