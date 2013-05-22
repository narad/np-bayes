package npbayes.wordseg.data

import com.google.common.collect.ImmutableList
import com.google.common.collect.ImmutableList.Builder
import com.google.common.collect.HashBiMap
import npbayes.WordType 

class Result(val tp: Double,val tr: Double, val bp: Double, val br: Double, val dp1: Double, val dr1: Double,
			val dp2: Double=0, val dr2: Double=0) {
  val tf = 2*tp*tr/(tp+tr)
  val bf = 2*bp*br/(bp+br)
  val df1 = 2*dp1*dr1/(dp1+dr1)
  val df2 = 2*dp2*dr2/(dp2+dr2)
  
  override def toString =
    "P "+tp+" R "+tr+" F "+tf+" BP "+bp+" BR "+br+" BF "+bf+" DP1 "+dp1+" DR1 "+dr1+" DF1 "+df1 + " DP2 "+dp2+" DR2 "+dr2 +" DF2 "+df2
} 


object SymbolSegTable {
  val mappingStringToSeg = HashBiMap.create[String,SegmentType]()
  def mappingSegToString = mappingStringToSeg.inverse
  var nextR: Int = 1
  def nSymbols = mappingStringToSeg.size
  def getNextR: Int = {
    nextR = (nextR+1)
    (nextR-1).toInt
  }
  
  def apply(x: String): SegmentType = {
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

object SymbolClassTable {
  val mappingStringToSeg = HashBiMap.create[String,SegmentType]()
  def mappingSegToString = mappingStringToSeg.inverse
  var nextR: Int = 1
  def nSymbols = mappingStringToSeg.size
  def getNextR: Int = {
    nextR = (nextR+1)
    (nextR-1).toInt
  }
  
  def apply(x: String): SegmentType = {
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
//  type WordType = Array[SegmentType]//ImmutableList[SegmentType]
  
  /**
   * convenience functions to build and display words
   */
  implicit def wToS (w: WordType,sep: String=""): String = {
    val res = new StringBuilder
    for (i: Int <- w){ 
//      res.append(SymbolTable(w.get(i)))
      res.append(SymbolSegTable(i))
    }
    res.mkString(sep)
  }
  
/*  implicit def sToW (s: String): WordType = {
    val res = new ImmutableList.Builder[SegmentType]
    for (c <- s.toList)
      res.add(SymbolTable(c.toString))
    res.build
  }*/

/*  def concat(w1: WordType, w2: WordType): WordType = 
    new ImmutableList.Builder[SegmentType].addAll(w1).addAll(w2).build()
  
  def suffix(w: WordType, suff: SegmentType) =
    new ImmutableList.Builder[SegmentType].addAll(w).add(suff).build()
  
  def prefix(pref: SegmentType, w: WordType) =
    new ImmutableList.Builder[SegmentType].add(pref).addAll(w).build()*/
  
  def segToWord(s: SegmentType): WordType = null
    //new WordType(new Array[Int](s),0,1)
//    ew ImmutableList.Builder[SegmentType].add(s).build()

} 